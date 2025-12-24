****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム            *
*    業務名　　　　　　　：　本部発注業務　　　　　　　　　　　*
*    モジュール名　　　　：　本発管理番号採番処理　　　　　　　*
*    作成日／更新日　　　：　2015/05/20                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　管理番号を採番し、各ファイルに更新*
*                            する。　　　　　　　　　　　　　　*
*    更新日　　　　　　　：　2015/07/03                        *
*    更新者　　　　　　　：　NAV INOUE                         *
*    更新概要　　　　　　：　取引先コード固定値　　　　　　　　*
*    　　　　　　　　　　　　　→　ＬＩＮＫ受取　に変更　　　　*
*    更新日　　　　　　　：　2015/07/07                        *
*    更新者　　　　　　　：　NAV INOUE                         *
*    更新概要　　　　　　：　レコード二重更新を修正　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY3841B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/05/20.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-STATUS.
*本発取込管理ファイル
     SELECT   HONKANF   ASSIGN    TO        DA-01-VI-HONKANL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HON-F01  HON-F02
                                            HON-F03  HON-F04
                                            HON-F05  HON-F06
                                            HON-F07
                        WITH  DUPLICATES
                        FILE      STATUS    HON-STATUS.
*管理番号マスタ
     SELECT   NFKANRF   ASSIGN    TO        DA-01-VI-NFKANRL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KAR-F01
                                            KAR-F02
                                            KAR-F03
                        WITH  DUPLICATES
                        FILE      STATUS    KAR-STATUS.
*取込数量確定ファイル
     SELECT   TRKAKUF   ASSIGN    TO        DA-01-VI-TRKAKUL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       OTS-F01  OTS-F02
                                            OTS-F03  OTS-F110
                                            OTS-F111 OTS-F112
                                            OTS-F113 OTS-F11H
                                            OTS-F115 OTS-F116
                                            OTS-F119 OTS-F11A
                                            OTS-F11B
                        WITH  DUPLICATES
                        FILE      STATUS    OTS-STATUS.
*取込箱数確定データ
     SELECT   TRHAKOF   ASSIGN    TO        DA-01-VI-TRHAKOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       OTH-F01  OTH-F02
                                            OTH-F03  OTH-F121
                                            OTH-F122 OTH-F123
                                            OTH-F124 OTH-F12B
                                            OTH-F126 OTH-F127
                                            OTH-F128
                        WITH  DUPLICATES
                        FILE      STATUS    OTH-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*
******************************************************************
*    本発取込管理ファイル
******************************************************************
 FD  HONKANF            LABEL RECORD   IS   STANDARD.
     COPY     HONKANF   OF        XFDLIB
              JOINING   HON       PREFIX.
******************************************************************
*    管理番号ファイル
******************************************************************
 FD  NFKANRF
                        LABEL RECORD   IS   STANDARD.
     COPY     NFKANRF   OF        XFDLIB
              JOINING   KAR  AS   PREFIX.
******************************************************************
*    取込数量訂正ファイル
******************************************************************
 FD  TRKAKUF            LABEL RECORD   IS   STANDARD.
     COPY     TRKAKUF   OF        XFDLIB
              JOINING   OTS       PREFIX.
******************************************************************
*    取込箱数確定ファイル
******************************************************************
 FD  TRHAKOF            LABEL RECORD   IS   STANDARD.
     COPY     TRHAKOF   OF        XFDLIB
              JOINING   OTH       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  END-FLG1                PIC  X(03)     VALUE  SPACE.
 01  END-FLG2                PIC  X(03)     VALUE  SPACE.
 01  TRKAKUF-CNT             PIC  9(08)     VALUE  ZERO.
 01  TRHAKOF-CNT             PIC  9(08)     VALUE  ZERO.
 01  HONKANF-CNT             PIC  9(08)     VALUE  ZERO.
 01  NFKANRF-CNT             PIC  9(08)     VALUE  ZERO.
 01  HONKANF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  NFKANRF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*01  WK-TOKCD                PIC  9(08)     VALUE  137607.
 01  WK-KANRINO              PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  JYO-STATUS        PIC  X(02).
     03  HON-STATUS        PIC  X(02).
     03  KAR-STATUS        PIC  X(02).
     03  OTS-STATUS        PIC  X(02).
     03  OTH-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3841B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3841B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3841B".
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
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC   9(08)  VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(06).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SAKUBACD          PIC   X(02).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-BUMON             PIC   X(04).
 01  PARA-KANRINO           PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE  PARA-JTIME
                                       PARA-TORICD
                                       PARA-SAKUBACD
                                       PARA-TANCD  PARA-BUMON
                                       PARA-KANRINO.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HONKANF.
     MOVE      "HONKANL1"   TO   AB-FILE.
     MOVE      HON-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFKANRF.
     MOVE      "NFKANRL1"   TO   AB-FILE.
     MOVE      KAR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TRKAKUF.
     MOVE      "TRKAKUL1"   TO   AB-FILE.
     MOVE      OTS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TRHAKOF.
     MOVE      "TRHAKOL1"   TO   AB-FILE.
     MOVE      OTH-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     I-O     HONKANF  NFKANRF.
     OPEN     I-O     TRKAKUF  TRHAKOF.
*
     DISPLAY  MSG-START UPON CONS.
*システム日付編集*
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
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*管理番号採番
     PERFORM   HJYOKEN-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*本発取込管理ファイル更新################
*****キー索引
     MOVE     PARA-JDATE         TO   HON-F01.
     MOVE     PARA-JTIME         TO   HON-F02.
     MOVE     ZERO               TO   HON-F03.
     MOVE     99999999           TO   HON-F04.
     MOVE     9999               TO   HON-F05.
*    MOVE     WK-TOKCD           TO   HON-F06.
     MOVE     PARA-TORICD        TO   HON-F06.
     MOVE     PARA-SAKUBACD      TO   HON-F07.
     PERFORM  HONKANF-READ-SEC.
*****存在しない場合、異常終了させる
*****存在する場合、管理番号を更新させる
     IF  HONKANF-INV-FLG = "INV"
         DISPLAY "## HONKANF INVALID ERROR ??? ##" UPON CONS
*↓TEST
     DISPLAY "   PARA-JDATE    = "  PARA-JDATE    UPON CONS
     DISPLAY "   PARA-JTIME    = "  PARA-JTIME    UPON CONS
     DISPLAY "   KANRI-NO      = "  "0"           UPON CONS
     DISPLAY "   バッチ日付    = "  "99999999"    UPON CONS
     DISPLAY "   バッチ時刻    = "  "9999"        UPON CONS
     DISPLAY "   PARA-TORICD   = " PARA-TORICD    UPON CONS
     DISPLAY "   PARA-SAKUBACD = " PARA-SAKUBACD  UPON CONS
*↑TEST
         MOVE    4000            TO   PROGRAM-STATUS
         STOP  RUN
     ELSE
         MOVE WK-KANRINO         TO   HON-F03
         REWRITE  HON-REC
         ADD  1                  TO   HONKANF-CNT
     END-IF.
*管理番号ファイル作成###################
*****同一キーが無いかチェック
     MOVE    "2"                 TO   KAR-F01.
     MOVE     WK-KANRINO         TO   KAR-F02.
     MOVE     99999999           TO   KAR-F031.
     MOVE     999999             TO   KAR-F032.
*    MOVE     137607             TO   KAR-F033.
     MOVE     PARA-TORICD        TO   KAR-F033.
     PERFORM  NFKANRF-READ-SEC.
*****存在しない場合、レコードを作成する
*****存在する場合、ＭＳＧを出力し次処理へ遷移
     IF  NFKANRF-INV-FLG = "INV"
         MOVE SPACE              TO   KAR-REC
         INITIALIZE                   KAR-REC
         MOVE  "2"               TO   KAR-F01
         MOVE  WK-KANRINO        TO   KAR-F02
         MOVE  99999999          TO   KAR-F031
         MOVE  999999            TO   KAR-F032
*        MOVE  137607            TO   KAR-F033
         MOVE  PARA-TORICD       TO   KAR-F033
         MOVE  PARA-TANCD        TO   KAR-F04
         MOVE  SYS-DATEW         TO   KAR-F98
         MOVE  WK-TIME(1:6)      TO   KAR-F99
         WRITE  KAR-REC
         ADD  1                  TO   NFKANRF-CNT
     ELSE
         DISPLAY "## TRKANRF NOT INAVDLI ERROR ##" UPON CONS
     END-IF.
*取込数量確定ファイル更新##############
     PERFORM  TRKAKUF-START-SEC.
     PERFORM  TRKAKUF-READ-SEC.
     MOVE     SPACE              TO   END-FLG1.
     PERFORM  UNTIL  END-FLG1 = "END"
       MOVE  WK-KANRINO          TO   OTS-F110
       REWRITE OTS-REC
       ADD     1                 TO   TRKAKUF-CNT
       PERFORM  TRKAKUF-READ-SEC
     END-PERFORM.
*取込箱数確定ファイル更新##############
     PERFORM  TRHAKOF-START-SEC.
     PERFORM  TRHAKOF-READ-SEC.
     MOVE     SPACE              TO   END-FLG2.
     PERFORM  UNTIL  END-FLG2 = "END"
       MOVE  WK-KANRINO          TO   OTH-F121
       REWRITE OTH-REC
       ADD     1                 TO   TRHAKOF-CNT
       PERFORM  TRHAKOF-READ-SEC
     END-PERFORM.
*
     MOVE     "END"              TO  END-FLG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　取込数量確定ファイル　スタート                            *
****************************************************************
 TRKAKUF-START-SEC     SECTION.
*
     MOVE    "TRKAKUF-START-SEC" TO  S-NAME.
*初期化
     MOVE     SPACE              TO  OTS-REC.
     INITIALIZE                      OTS-REC.
*キーセット
     MOVE     "2"                TO  OTS-F01.
     MOVE     PARA-JDATE         TO  OTS-F02.
     MOVE     PARA-JTIME         TO  OTS-F03.
     START  TRKAKUF KEY IS  >=  OTS-F01  OTS-F02  OTS-F03
                                OTS-F110 OTS-F111 OTS-F112
                                OTS-F113 OTS-F11H OTS-F115
                                OTS-F116 OTS-F119 OTS-F11A
                                OTS-F11B
       INVALID
         DISPLAY "## TRKAKUF NOT START ERR ??? ##" UPON CONS
         MOVE    4000            TO   PROGRAM-STATUS
         STOP  RUN
     END-START.
*
 TRKAKUF-START-EXIT.
     EXIT.
****************************************************************
*　　取込数量確定ファイル　読込　　　                          *
****************************************************************
 TRKAKUF-READ-SEC      SECTION.
*
     MOVE    "TRKAKUF-READ-SEC"  TO  S-NAME.
*
     READ  TRKAKUF
           NEXT  AT  END
           MOVE  "END"           TO  END-FLG1
           GO                    TO  TRKAKUF-READ-EXIT
     END-READ.
*実行日付、時刻チェック
     IF    PARA-JDATE   =   OTS-F02
     AND   PARA-JTIME   =   OTS-F03
           CONTINUE
     ELSE
           MOVE  "END"           TO  END-FLG1
     END-IF.
*2015/07/07↓
     IF    WK-KANRINO   =   OTS-F110
           MOVE  "END"           TO  END-FLG1
     END-IF.
*2015/07/07↑
*
 TRKAKUF-READ-EXIT.
     EXIT.
****************************************************************
*　　取込箱数確定ファイル　スタート                            *
****************************************************************
 TRHAKOF-START-SEC     SECTION.
*
     MOVE    "TRHAKOF-START-SEC" TO  S-NAME.
*初期化
     MOVE     SPACE              TO  OTH-REC.
     INITIALIZE                      OTH-REC.
*キーセット
     MOVE     "2"                TO  OTH-F01.
     MOVE     PARA-JDATE         TO  OTH-F02.
     MOVE     PARA-JTIME         TO  OTH-F03.
     START  TRHAKOF KEY IS  >=  OTH-F01  OTH-F02  OTH-F03
                                OTH-F121 OTH-F122 OTH-F123
                                OTH-F124 OTH-F12B OTH-F126
                                OTH-F127 OTH-F128
       INVALID
         DISPLAY "## TRHAKOF NOT START ERR ??? ##" UPON CONS
         MOVE    4000            TO   PROGRAM-STATUS
         STOP  RUN
     END-START.
*
 TRHAKOF-START-EXIT.
     EXIT.
****************************************************************
*　　取込箱数確定ファイル　読込　　　                          *
****************************************************************
 TRHAKOF-READ-SEC      SECTION.
*
     MOVE    "TRHAKOF-READ-SEC"  TO  S-NAME.
*
     READ  TRHAKOF
           NEXT  AT  END
           MOVE  "END"           TO  END-FLG2
           GO                    TO  TRHAKOF-READ-EXIT
     END-READ.
*実行日付、時刻チェック
     IF    PARA-JDATE   =   OTH-F02
     AND   PARA-JTIME   =   OTH-F03
           CONTINUE
     ELSE
           MOVE  "END"           TO  END-FLG2
     END-IF.
*2015/07/07↓
     IF    WK-KANRINO   =   OTH-F121
           MOVE  "END"           TO  END-FLG2
     END-IF.
*2015/07/07↑
*TEST↑
*
 TRHAKOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"          TO  S-NAME.
*
     DISPLAY  "# HONKANF-CNT = " HONKANF-CNT UPON CONS.
     DISPLAY  "# NFKANRF-CNT = " NFKANRF-CNT UPON CONS.
     DISPLAY  "# TRKAKUF-CNT = " TRKAKUF-CNT UPON CONS.
     DISPLAY  "# TRHAKOF-CNT = " TRHAKOF-CNT UPON CONS.
*
     CLOSE     HONKANF  NFKANRF  TRKAKUF  TRHAKOF.
*管理番号を後続処理へ渡す.
     MOVE      WK-KANRINO        TO  PARA-KANRINO.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　本発取込管理ファイル読込
****************************************************************
 HONKANF-READ-SEC        SECTION.
*
     MOVE   "HONKANF-READ-SEC"  TO   S-NAME.
*
     READ  HONKANF
           INVALID
           MOVE  "INV"          TO   HONKANF-INV-FLG
           NOT  INVALID
           MOVE  SPACE          TO   HONKANF-INV-FLG
     END-READ.
*
 HONKANF-READ-EXIT.
     EXIT.
****************************************************************
*　　管理番号ファイル
****************************************************************
 NFKANRF-READ-SEC        SECTION.
*
     MOVE   "NFKANRF-READ-SEC"  TO   S-NAME.
*
     READ  NFKANRF
           INVALID
           MOVE  "INV"          TO   NFKANRF-INV-FLG
           NOT  INVALID
           MOVE  SPACE          TO   NFKANRF-INV-FLG
     END-READ.
*
 NFKANRF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　条件ファイル読込　　　　　　　　　　　　　　　　*
****************************************************************
 HJYOKEN-READ-SEC        SECTION.
*
     MOVE   "HJYOKEN-READ-SEC"  TO   S-NAME.
*
     OPEN   I-O   HJYOKEN.
*
     MOVE   "99"           TO        JYO-F01.
     MOVE   "NAFUKOH"      TO        JYO-F02.
     READ   HJYOKEN
       INVALID
            DISPLAY "## HJYOKEN INVALID KEY 99 NAFUKO" UPON CONS
            MOVE      4000         TO   PROGRAM-STATUS
            STOP  RUN
     END-READ.
*
     ADD    1              TO        JYO-F04.
*
     IF     JYO-F04  >  JYO-F06
            MOVE  JYO-F05  TO        JYO-F04
            MOVE  JYO-F04  TO        WK-KANRINO
     ELSE
            MOVE  JYO-F04  TO        WK-KANRINO
     END-IF.
*
     DISPLAY "#KANRINO = " WK-KANRINO " #" UPON CONS.
*
     REWRITE JYO-REC.
*
     CLOSE  HJYOKEN.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
