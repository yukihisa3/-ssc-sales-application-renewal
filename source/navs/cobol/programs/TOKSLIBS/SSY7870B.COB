****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ロイヤルＨＣ　新ＥＤＩ対応　　　　*
*    業務名　　　　　　　：　ロイヤルＨＣ　新ＥＤＩ対応　　　　*
*    モジュール名　　　　：　納品予定データ送信形式変換１      *
*    作成日／更新日　　　：　2008/10/27                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
*    作成日／更新日　　　：　2014/01/28 *$$$                   *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　データ内の消費税率を使用に変更　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7870B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/10/27.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ロイヤルＨＣ　納品予定データ
     SELECT   RHSYUKF   ASSIGN    TO        DA-01-VI-RHSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RHS-F01   RHS-F02
                                            RHS-F03   RHS-F04
                                            RHS-F05   RHS-F06
                                            RHS-F07   RHS-F08
                        FILE      STATUS    RCS-STATUS.
*ロイヤルＨＣ　発注情報データ
     SELECT   RHJOHOF   ASSIGN    TO        DA-01-VI-RHJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RHJ-F01   RHJ-F02
                                            RHJ-F03   RHJ-F04
                                            RHJ-F05   RHJ-F06
                                            RHJ-F07   RHJ-F08
                        FILE      STATUS    RCJ-STATUS.
*ロイヤルＨＣ　送信情報ファイル
     SELECT   RHSENDF   ASSIGN    TO        DA-01-VI-RHSENDL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SND-F01   SND-F02
                                            SND-F03   SND-F04
                                            SND-F05   SND-F06
                                            SND-F07   SND-F08
                        FILE      STATUS    SND-STATUS.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN              DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        STATUS              JYO-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ロイヤルＨＣ　納品予定ファイル
******************************************************************
 FD  RHSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     RHSYUKF   OF        XFDLIB
              JOINING   RHS       PREFIX.
******************************************************************
*    ロイヤルＨＣ　発注情報データ
******************************************************************
 FD  RHJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     RHJOHOF   OF        XFDLIB
              JOINING   RHJ       PREFIX.
******************************************************************
*    ロイヤルＨＣ　送信情報ファイル
******************************************************************
 FD  RHSENDF            LABEL RECORD   IS   STANDARD.
     COPY     RHSENDF   OF        XFDLIB
              JOINING   SND       PREFIX.
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RHJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SND-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-TAIL-02              PIC  9(11)     VALUE  ZERO.
 01  WK-TAIL-03              PIC  9(11)     VALUE  ZERO.
 01  WK-RHS-C15              PIC  9(08)V9(02)  VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SND-STATUS        PIC  X(02).
     03  RCS-STATUS        PIC  X(02).
     03  RCJ-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7870B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7870B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7870B".
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
     COPY   RHSYUKF  OF XFDLIB  JOINING   WRK  AS   PREFIX.
*消費税情報
 01  WK-SYOHIZEI.
     03  WK-SYOHI01         PIC   9(02)V9(02)   VALUE  ZERO.
     03  WK-SYOHI02         PIC   9(02)V9(02)   VALUE  ZERO.
     03  WK-SYOHI03         PIC   9(08)         VALUE  ZERO.
     03  WK-SYOHI04         PIC   9(02)V9(02)   VALUE  ZERO.
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
                        PROCEDURE   RHSYUKF.
     MOVE      "RHSYUKL2"   TO   AB-FILE.
     MOVE      RCS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RHSENDF.
     MOVE      "RHSENDL1"   TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RHJOHOF.
     MOVE      "RHJOHOL1"   TO   AB-FILE.
     MOVE      RCJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "JYOMEN1 "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
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
     OPEN     I-O       RHSYUKF   RHJOHOF.
     OPEN     I-O       RHSENDF.
     OPEN     INPUT     HJYOKEN.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT  WK-RHS-C15.
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
*----<< 消費税率取得 >>-*
     MOVE     99        TO   JYO-F01.
     MOVE     "ZEI"     TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF       HJYOKEN-INV-FLG = SPACE
              MOVE      JYO-F04   TO   WK-SYOHI01
              MOVE      JYO-F05   TO   WK-SYOHI02
              MOVE      JYO-F06   TO   WK-SYOHI03
              MOVE      JYO-F07   TO   WK-SYOHI04
     ELSE
              DISPLAY NC"条件Ｆ異常！！"  UPON CONS
              DISPLAY NC"ＫＥＹ　＝　　"  JYO-F01 ":" JYO-F02
                                          UPON CONS
              MOVE    4000        TO   PROGRAM-STATUS
              STOP  RUN
     END-IF.
*    ロイヤルＨＣ　　　　　出荷情報スタート
     MOVE     SPACE          TO   RHS-REC.
     INITIALIZE                   RHS-REC.
     MOVE     PARA-JDATE     TO   RHS-F01.
     MOVE     PARA-JTIME     TO   RHS-F02.
     MOVE     PARA-TORICD    TO   RHS-F03.
     MOVE     PARA-SOKO      TO   RHS-F04.
     MOVE     ZERO           TO   RHS-F08.
     START    RHSYUKF   KEY  >=   RHS-F01   RHS-F02
                                  RHS-F03   RHS-F04
                                  RHS-F05   RHS-F06
                                  RHS-F07   RHS-F08
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ロイヤルＨＣ　　　　　出荷情報保存データ読込み
     PERFORM RHSYUKF-READ-SEC.
*    レコードをワークに転送
     MOVE     RHS-REC             TO   WRK-REC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷情報データ　ＲＥＡＤ　　　　　　　　　　　　*
****************************************************************
 RHSYUKF-READ-SEC    SECTION.
*
     READ     RHSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  RHSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
     IF    READ-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*    送信ＦＬＧのチェック
     IF       RHS-F11  =  "1"
              GO                 TO   RHSYUKF-READ-SEC
     END-IF.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  RHS-F01
     AND      PARA-JTIME  =  RHS-F02
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  RHSYUKF-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-SOKO   =  RHS-F04
                       CONTINUE
              ELSE
                       GO        TO   RHSYUKF-READ-SEC
              END-IF
     END-IF.
*納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-NOUDT  =  RHS-F08
                       CONTINUE
              ELSE
                       GO        TO   RHSYUKF-READ-SEC
              END-IF
     END-IF.
*
 RHSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
     IF       RHS-F01  NOT =  WRK-F01
     OR       RHS-F02  NOT =  WRK-F02
     OR       RHS-F03  NOT =  WRK-F03
     OR       RHS-F04  NOT =  WRK-F04
     OR       RHS-F05  NOT =  WRK-F05
     OR       RHS-F06  NOT =  WRK-F06
*****OR       RHS-F07  NOT =  WRK-F07
     OR       RHS-F08  NOT =  WRK-F08
*             ヘッダ出力処理
              MOVE     SPACE       TO   SND-REC
              INITIALIZE                SND-REC
              MOVE     WRK-F01     TO   SND-F01
              MOVE     WRK-F02     TO   SND-F02
              MOVE     WRK-F03     TO   SND-F03
              MOVE     WRK-F04     TO   SND-F04
              MOVE     WRK-F05     TO   SND-F05
              MOVE     WRK-F06     TO   SND-F06
              MOVE     ZERO        TO   SND-F07
              MOVE     WRK-F08     TO   SND-F08
*$$$2014/01/28 NAV ST 明細内の消費税率で消費是金額を計算する
**************IF    WK-SYOHI03  >=  WRK-F08
*                   COMPUTE WRK-B11 ROUNDED =
*                           WK-RHS-C15  *  WK-SYOHI04
*             ELSE
*                   COMPUTE WRK-B11 ROUNDED =
*                           WK-RHS-C15  *  WK-SYOHI02
**************END-IF
**************DISPLAY "ZEIRITU = " WRK-C21 UPON CONS
              COMPUTE WRK-B11 ROUNDED =
                      WK-RHS-C15  *  ( WRK-C21 / 100)
*$$$2014/01/28 NAV ED 明細内の消費税率で消費是金額を計算する
              MOVE     WRK-B00     TO   SND-F14
              WRITE    SND-REC
              MOVE     ZERO        TO   WK-RHS-C15
     END-IF.
*    送信ファイル出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*    レコードをワークに転送
     MOVE     RHS-REC             TO   WRK-REC.
*    明細を出力する。
     MOVE     RHS-F01             TO   SND-F01.
     MOVE     RHS-F02             TO   SND-F02.
     MOVE     RHS-F03             TO   SND-F03.
     MOVE     RHS-F04             TO   SND-F04.
     MOVE     RHS-F05             TO   SND-F05.
     MOVE     RHS-F06             TO   SND-F06.
     MOVE     RHS-F07             TO   SND-F07.
     MOVE     RHS-F08             TO   SND-F08.
     MOVE     RHS-C00             TO   SND-F14.
*
*    原価金額集計
     ADD      RHS-C15             TO   WK-RHS-C15.
*    出荷送信情報ファイル書き込み
     WRITE    SND-REC.
     ADD      1                   TO   SND-CNT.
*情報データにも書込み
     MOVE     SPACE               TO   RHJ-REC.
     INITIALIZE                        RHJ-REC.
     MOVE     RHS-F01             TO   RHJ-F01.
     MOVE     RHS-F02             TO   RHJ-F02.
     MOVE     RHS-F03             TO   RHJ-F03.
     MOVE     RHS-F04             TO   RHJ-F04.
     MOVE     RHS-F05             TO   RHJ-F05.
     MOVE     RHS-F06             TO   RHJ-F06.
     MOVE     RHS-F07             TO   RHJ-F07.
     MOVE     RHS-F08             TO   RHJ-F08.
     READ     RHJOHOF  INVALID
              MOVE     "INV"      TO   RHJOHOF-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   RHJOHOF-INV-FLG
     END-READ.
*
     IF       RHJOHOF-INV-FLG = SPACE
              MOVE     SYS-DATEW           TO   RHJ-F12
              MOVE     "1"                 TO   RHJ-F11
              REWRITE  RHJ-REC
     END-IF.
*送信情報セット
     MOVE     SYS-DATEW           TO   RHS-F12.
     MOVE     "1"                 TO   RHS-F11.
     REWRITE  RHS-REC.
*
 MAIN010.
*    ロイヤルＨＣ　出荷情報保存データ読込み
     PERFORM RHSYUKF-READ-SEC.
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
     IF        SND-CNT  >  ZERO
*             ヘッダ出力処理
              MOVE     SPACE       TO   SND-REC
              INITIALIZE                SND-REC
              MOVE     WRK-F01     TO   SND-F01
              MOVE     WRK-F02     TO   SND-F02
              MOVE     WRK-F03     TO   SND-F03
              MOVE     WRK-F04     TO   SND-F04
              MOVE     WRK-F05     TO   SND-F05
              MOVE     WRK-F06     TO   SND-F06
              MOVE     ZERO        TO   SND-F07
              MOVE     WRK-F08     TO   SND-F08
*$$$2014/01/28 NAV ST 明細内の消費税率で消費是金額を計算する
**************IF    WK-SYOHI03  >=  WRK-F08
*                   COMPUTE WRK-B11 ROUNDED =
*                           WK-RHS-C15  *  WK-SYOHI04
*             ELSE
*                   COMPUTE WRK-B11 ROUNDED =
*                           WK-RHS-C15  *  WK-SYOHI02
**************END-IF
              COMPUTE WRK-B11 ROUNDED =
                      WK-RHS-C15  *  ( WRK-C21 / 100)
*$$$2014/01/28 NAV ED 明細内の消費税率で消費是金額を計算する
              MOVE     WRK-B00     TO   SND-F14
              WRITE    SND-REC
     END-IF.
*
     DISPLAY "ﾛｲﾔﾙHC ｼｭｯｶ   READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ﾛｲﾔﾙHC ﾘｯｸｼｭｯｶ ｿｳｼﾝCNT = " SND-CNT    UPON CONS.
*
     CLOSE    RHSYUKF  RHJOHOF  RHSENDF  HJYOKEN.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    条件ファイル　 READ                          *
****************************************************************
 HJYOKEN-READ-SEC       SECTION.
*
     READ     HJYOKEN   INVALID
              MOVE      "INV"          TO   HJYOKEN-INV-FLG
              NOT  INVALID
              MOVE      SPACE          TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
