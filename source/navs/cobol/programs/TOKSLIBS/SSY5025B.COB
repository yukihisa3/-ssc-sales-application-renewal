****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理（トステムビバ新EDI）
*    業務名　　　　　　　：　ベンダーオンライン            　
*    モジュール名　　　　：　納品予定送信フォーマット変換      *
*    作成日／更新日　　　：　2010/03/26                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報データより抽出する。    *
*    作成日／更新日　　　：　2021/10/07                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　伝票番号ブレイクキーに他キー追加  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5025B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/03/26.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*出荷情報ファイル
     SELECT   KHSYUKF   ASSIGN    TO        DA-01-VI-KHSYUKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KHS-F01   KHS-F02
                                            KHS-F03   KHS-F04
                                            KHS-F08   KHS-F05
                                            KHS-F06   KHS-F07
                        FILE      STATUS    KHS-STATUS.
*基本情報ファイル
     SELECT   KHJOHOF   ASSIGN    TO        DA-01-VI-KHJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KHJ-F01   KHJ-F02
                                            KHJ-F03   KHJ-F04
                                            KHJ-F05   KHJ-F06
                                            KHJ-F07   KHJ-F08
                        FILE      STATUS    KHJ-STATUS.
*トステムビバ送信ファイル（新）
     SELECT   SNDKHTSB  ASSIGN    TO        DA-01-S-SNDKHTSB
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SND-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    出荷情報ファイル
******************************************************************
 FD  KHSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     KHSYUKF   OF        XFDLIB
              JOINING   KHS       PREFIX.
******************************************************************
*    基本情報ファイル
******************************************************************
 FD  KHJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     KHJOHOF   OF        XFDLIB
              JOINING   KHJ       PREFIX.
******************************************************************
*    トステムビバ送信ファイル（新）
******************************************************************
 FD  SNDKHTSB
                        BLOCK CONTAINS 1 RECORDS.
 01  SND-REC            PIC  X(128).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  KHJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SND-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
*#2021/10/07 NAV ST
 01  WK-TENPO                PIC  9(05)     VALUE  ZERO.
 01  WK-NOUDT                PIC  9(08)     VALUE  ZERO.
*#2021/10/07 NAV ED
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SND-STATUS        PIC  X(02).
     03  KHS-STATUS        PIC  X(02).
     03  KHJ-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY5025B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5025B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5025B".
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
                        PROCEDURE   KHSYUKF.
     MOVE      "KHSYUKL2"    TO   AB-FILE.
     MOVE      KHS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDKHTSB.
     MOVE      "SNDKHTSB"   TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KHJOHOF.
     MOVE      "KHJOHOL1"   TO   AB-FILE.
     MOVE      KHJ-STATUS   TO   AB-STS.
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
     OPEN     I-O       KHSYUKF   KHJOHOF.
     OPEN     EXTEND    SNDKHTSB.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
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
*    出荷情報ファイルスタート
     MOVE     SPACE          TO   KHS-REC.
     INITIALIZE                   KHS-REC.
     MOVE     PARA-JDATE     TO   KHS-F01.
     MOVE     PARA-JTIME     TO   KHS-F02.
     MOVE     PARA-TORICD    TO   KHS-F03.
     MOVE     PARA-SOKO      TO   KHS-F04.
     MOVE     PARA-NOUDT     TO   KHS-F08.
     START    KHSYUKF   KEY  >=   KHS-F01   KHS-F02
                                  KHS-F03   KHS-F04
                                  KHS-F08
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    出荷情報ファイル読込み
     PERFORM KHSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷情報ファイル読込み処理　　　　　　　　　　　*
****************************************************************
 KHSYUKF-READ-SEC    SECTION.
*
     READ     KHSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KHSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
     IF    READ-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*    送信ＦＬＧのチェック
     IF       KHS-F11  =  "1"
              GO                 TO   KHSYUKF-READ-SEC
     END-IF.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  KHS-F01
     AND      PARA-JTIME  =  KHS-F02
     AND      PARA-TORICD =  KHS-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  KHSYUKF-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-SOKO   =  KHS-F04
                       CONTINUE
              ELSE
                       GO        TO   KHSYUKF-READ-SEC
              END-IF
     END-IF.
*納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-NOUDT  =  KHS-F08
                       CONTINUE
              ELSE
                       GO        TO   KHSYUKF-READ-SEC
              END-IF
     END-IF.
*
 KHSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*送信ファイル出力
*    ヘッダ部出力（伝票番号ブレーク時）
     IF       KHS-F06    =   WK-DENNO
*#2021/10/07 NAV ST ブレイク条件追加
     AND      KHS-F05    =   WK-TENPO
     AND      KHS-F08    =   WK-NOUDT
*#2021/10/07 NAV ED
              CONTINUE
     ELSE
              MOVE     SPACE      TO   SND-REC
              INITIALIZE               SND-REC
              MOVE     KHS-HED    TO   SND-REC
              WRITE    SND-REC
*
              MOVE     SPACE      TO   SND-REC
              INITIALIZE               SND-REC
              MOVE     KHS-HME    TO   SND-REC
              WRITE    SND-REC
*
              ADD      2          TO   SND-CNT
              MOVE     KHS-F06    TO   WK-DENNO
**************#2021/10/07 NAV ST
              MOVE     KHS-F05    TO   WK-TENPO
              MOVE     KHS-F08    TO   WK-NOUDT
**************#2021/10/07 NAV ED
     END-IF.
*
*    明細部出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
     MOVE     KHS-MEI             TO   SND-REC.
     WRITE    SND-REC.
     ADD      1                   TO   SND-CNT.
*
*基本情報データにも書込み
     MOVE     SPACE               TO   KHJ-REC.
     INITIALIZE                        KHJ-REC.
     MOVE     KHS-F01             TO   KHJ-F01.
     MOVE     KHS-F02             TO   KHJ-F02.
     MOVE     KHS-F03             TO   KHJ-F03.
     MOVE     KHS-F04             TO   KHJ-F04.
     MOVE     KHS-F05             TO   KHJ-F05.
     MOVE     KHS-F06             TO   KHJ-F06.
     MOVE     KHS-F07             TO   KHJ-F07.
     MOVE     KHS-F08             TO   KHJ-F08.
     READ     KHJOHOF  INVALID
              MOVE     "INV"      TO   KHJOHOF-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   KHJOHOF-INV-FLG
     END-READ.
*
     IF       KHJOHOF-INV-FLG = SPACE
              MOVE     SYS-DATEW           TO   KHJ-F12
              MOVE     "1"                 TO   KHJ-F11
              REWRITE  KHJ-REC
     END-IF.
*出荷情報データ 送信情報セット
     MOVE     SYS-DATEW           TO   KHS-F12.
     MOVE     "1"                 TO   KHS-F11.
     REWRITE  KHS-REC.
*
 MAIN010.
*    出荷情報ファイル読込み
     PERFORM KHSYUKF-READ-SEC.
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
     DISPLAY "ﾄｽﾃﾑﾋﾞﾊﾞ ｼｭｯｶ   READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ﾄｽﾃﾑﾋﾞﾊﾞ ｼｭｯｶ ｿｳｼﾝ   CNT = " SND-CNT    UPON CONS.
*
     CLOSE    KHSYUKF  KHJOHOF  SNDKHTSB.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
