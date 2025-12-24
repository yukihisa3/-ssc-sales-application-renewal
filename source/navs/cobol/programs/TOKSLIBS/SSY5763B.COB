****************************************************************
*    顧客名　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　：　グッデイＥＤＩ
*    業務名　　　　　　：　出荷業務
*    モジュール名　　　：　出荷送信データ作成
*    作成日／更新日　　：　2014/06/16
*    作成者／更新者　　：　NAV
*    処理概要　　　　　：　受け取ったパラメタと合致するデータを
*                          出荷情報ファイルより作成する。
*    更新履歴          ：
*    　　　　　　　　  　
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5763B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/06/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*グッデイ　出荷情報ファイル
     SELECT   GDSYUKL2  ASSIGN    TO        DA-01-VI-GDSYUKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SYU-F01   SYU-F02
                                            SYU-F03   SYU-F04
                                            SYU-F08   SYU-F05
                                            SYU-F06   SYU-F07
                        FILE      STATUS    SYU-STATUS.
*グッデイ　基本情報ファイル
     SELECT   GDJOHOL2  ASSIGN    TO        DA-01-VI-GDJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F01   JOH-F02
                                            JOH-F03   JOH-F04
                                            JOH-F08   JOH-F05
                                            JOH-F06   JOH-F07
                        FILE      STATUS    JOH-STATUS.
*グッデイ　出荷送信ファイル（ヘッダ）
     SELECT   GDSNDHD   ASSIGN    TO        DA-01-S-GDSNDHD
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    SHD-STATUS.
*グッデイ　出荷送信ファイル（明細）
     SELECT   GDSNDMS   ASSIGN    TO        DA-01-S-GDSNDMS
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    SMS-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    グッデイ　基本情報ファイル
******************************************************************
 FD  GDSYUKL2            LABEL RECORD   IS   STANDARD.
     COPY     GDSYUKL2   OF        XFDLIB
              JOINING    SYU       PREFIX.
******************************************************************
*    グッデイ　基本情報ファイル
******************************************************************
 FD  GDJOHOL2            LABEL RECORD   IS   STANDARD.
     COPY     GDJOHOL2   OF        XFDLIB
              JOINING    JOH       PREFIX.
******************************************************************
*    グッデイ　出荷送信ファイル（ヘッダ）
******************************************************************
 FD  GDSNDHD            BLOCK CONTAINS  1   RECORDS.
     COPY     GDSNDHD   OF        XFDLIB
              JOINING   SHD       PREFIX.
******************************************************************
*    グッデイ　出荷送信ファイル（明細）
******************************************************************
 FD  GDSNDMS            BLOCK CONTAINS  1   RECORDS.
     COPY     GDSNDMS   OF        XFDLIB
              JOINING   SMS       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  GDJOHOL2-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  DEN-CNT             PIC  9(08)     VALUE  ZERO.
     03  SHD-CNT             PIC  9(08)     VALUE  ZERO.
     03  SMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-TAIL-02              PIC  9(11)     VALUE  ZERO.
 01  WK-TAIL-03              PIC  9(11)     VALUE  ZERO.
 01  WK-SYU-C15              PIC  9(08)V9(02)  VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SYU-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  SHD-STATUS        PIC  X(02).
     03  SMS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY5763B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5763B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5763B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*出荷情報一次退避
     COPY   GDSYUKL2  OF XFDLIB  JOINING   WRK  AS   PREFIX.
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDSYUKL2.
     MOVE      "SYUYUKL2"   TO   AB-FILE.
     MOVE      SYU-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDJOHOL2.
     MOVE      "GDJOHOL2"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDSNDHD.
     MOVE      "GDSNDHD"    TO   AB-FILE.
     MOVE      SHD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDSNDMS.
     MOVE      "GDSNDMS"    TO   AB-FILE.
     MOVE      SMS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       GDSYUKL2   GDJOHOL2.
     OPEN     OUTPUT    GDSNDHD    GDSNDMS.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT  WK-SYU-C15.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*    グッデイ　　　　　出荷情報スタート
     MOVE     SPACE          TO   SYU-REC.
     INITIALIZE                   SYU-REC.
     MOVE     PARA-JDATE     TO   SYU-F01.
     MOVE     PARA-JTIME     TO   SYU-F02.
     MOVE     PARA-TORICD    TO   SYU-F03.
     MOVE     PARA-SOKO      TO   SYU-F04.
     MOVE     ZERO           TO   SYU-F08.
     START    GDSYUKL2   KEY  >=  SYU-F01   SYU-F02
                                  SYU-F03   SYU-F04
                                  SYU-F08   SYU-F05
                                  SYU-F06   SYU-F07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    グッデイ　　　　　出荷情報保存データ読込み
     PERFORM GDSYUKL2-READ-SEC.
*    レコードをワークに転送
*    MOVE     SYU-REC             TO   WRK-REC.
     INITIALIZE                   WRK-REC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷情報データ　ＲＥＡＤ　　　　　　　　　　　　*
****************************************************************
 GDSYUKL2-READ-SEC    SECTION.
*
     READ     GDSYUKL2
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  GDSYUKL2-READ-EXIT
*             NOT AT END
*                 ADD       1       TO  READ-CNT
     END-READ.
*
*    バッチ番号のチェック
     IF       PARA-JDATE   =  SYU-F01
     AND      PARA-JTIME   =  SYU-F02
     AND      PARA-TORICD  =  SYU-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  GDSYUKL2-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-SOKO   =  SYU-F04
                       CONTINUE
              ELSE
                       GO        TO   GDSYUKL2-READ-SEC
              END-IF
     END-IF.
*納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-NOUDT  =  SYU-F08
                       CONTINUE
              ELSE
                       GO        TO   GDSYUKL2-READ-SEC
              END-IF
     END-IF.
*
     ADD      1       TO  READ-CNT
     IF    READ-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*
*    送信ＦＬＧのチェック
     IF       SYU-F12  =  "1"
              GO                 TO   GDSYUKL2-READ-SEC
     END-IF.
*
 GDSYUKL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*   伝票枚数カウントアップ
     IF       SYU-F01  NOT =  WRK-F01  OR
              SYU-F02  NOT =  WRK-F02  OR
              SYU-F03  NOT =  WRK-F03  OR
              SYU-F04  NOT =  WRK-F04  OR
              SYU-F05  NOT =  WRK-F05  OR
              SYU-F06  NOT =  WRK-F06  OR
              SYU-F08  NOT =  WRK-F08
              ADD      1          TO   DEN-CNT
     END-IF.
*
*出荷送信ファイル(明細)出力
     MOVE     SPACE               TO   SMS-REC.
     INITIALIZE                        SMS-REC.
* レコードをワークに保管
     MOVE     SYU-REC             TO   WRK-REC.
* 項目セット
*   レコード区分
     MOVE     "D"                 TO   SMS-M01.
*   伝票番号
*****MOVE     SYU-F06             TO   SMS-M02.
     MOVE     SYU-A02             TO   SMS-M02.
*   行番号
     MOVE     SYU-F07             TO   SMS-M03.
*   店舗ＣＤ
     MOVE     SYU-F05             TO   SMS-M04.
*   伝票区分
     MOVE     SYU-A04             TO   SMS-M05.
*   発注日
     MOVE     SYU-A06             TO   SMS-M06.
*   出荷日
     MOVE     SYS-DATEW           TO   SMS-M07.
*   店舗納品予定日
     MOVE     SYU-A08             TO   SMS-M08.
*   センター納品日
     MOVE     SYU-A23             TO   SMS-M09.
*   仕入先ＣＤ
     MOVE     SYU-A09             TO   SMS-M10.
*   倉庫区分
     MOVE     SYU-A10             TO   SMS-M11.
*   品番ＣＤ
     MOVE     SYU-B03             TO   SMS-M12.
*   出荷数量
     MOVE     SYU-F09             TO   SMS-M13.
*   ＳＣＭラベルコード
     MOVE     ZERO                TO   SMS-M141.
     MOVE     ZERO                TO   SMS-M142.
*
*   出荷送信ファイル(明細)出力
     WRITE    SMS-REC.
     ADD      1                   TO   SMS-CNT.
*
*基本情報ファイル更新
     MOVE     SPACE               TO   JOH-REC.
     INITIALIZE                        JOH-REC.
     MOVE     SYU-F01             TO   JOH-F01.
     MOVE     SYU-F02             TO   JOH-F02.
     MOVE     SYU-F03             TO   JOH-F03.
     MOVE     SYU-F04             TO   JOH-F04.
     MOVE     SYU-F08             TO   JOH-F08.
     MOVE     SYU-F05             TO   JOH-F05.
     MOVE     SYU-F06             TO   JOH-F06.
     MOVE     SYU-F07             TO   JOH-F07.
     READ     GDJOHOL2  INVALID
              MOVE     "INV"      TO   GDJOHOL2-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   GDJOHOL2-INV-FLG
     END-READ.
*
     IF       GDJOHOL2-INV-FLG = SPACE
              MOVE     SYS-DATEW           TO   JOH-F13
              MOVE     "1"                 TO   JOH-F12
              REWRITE  JOH-REC
     END-IF.
*出荷情報ファイル更新
     MOVE     SYS-DATEW           TO   SYU-F13.
     MOVE     "1"                 TO   SYU-F12.
     REWRITE  SYU-REC.
*
 MAIN010.
*   グッデイ　出荷情報保存データ読込み
     PERFORM GDSYUKL2-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
*   出荷送信ファイル(ヘッダ)出力処理
     IF       SMS-CNT  >  ZERO
              MOVE     1           TO   SHD-CNT
              MOVE     SPACE       TO   SHD-REC
              INITIALIZE                SHD-REC
*           レコード区分
              MOVE     "A"         TO   SHD-H01
*           レコード件数
              COMPUTE  SHD-H02  =  SHD-CNT + SMS-CNT
*           伝票枚数
              MOVE     DEN-CNT     TO   SHD-H03
*           出荷送信ファイル(ヘッダ)出力
              WRITE    SHD-REC
     END-IF.
*
     DISPLAY NC"出荷情報ファイル　　読込＝" READ-CNT UPON CONS.
     DISPLAY NC"送信ファイルヘッダ　出力＝" SHD-CNT  UPON CONS.
     DISPLAY NC"送信ファイル明細　　出力＝" SMS-CNT  UPON CONS.
*
     CLOSE    GDSYUKL2  GDJOHOL2  GDSNDHD  GDSNDMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
