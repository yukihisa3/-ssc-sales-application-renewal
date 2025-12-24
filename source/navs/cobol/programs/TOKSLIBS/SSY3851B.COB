****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援システム　　　　　　*
*    モジュール名　　　　：　本発ＥＸＣＥＬＤＴ取消処理　　　　*
*    作成日／作成者　　　：　2015/06/26 NAV TAKAHASHI          *
*    処理概要　　　　　　：　本発ＥＸＣＥＬ取込を行ったデータ　*
*                            の取消を行う。　　　　　　　　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY3851B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*本発基本情報ファイル
     SELECT   NHJOHOF   ASSIGN    TO        DA-01-VI-NHJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JHO-F01   JHO-F05
                                            JHO-F06   JHO-F07
                                            JHO-F08   JHO-F09
                        FILE      STATUS    JHO-ST.
*本発売上伝票ファイル
     SELECT   NHTDENF   ASSIGN    TO        DA-01-VI-NHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE STATUS    IS   DEN-ST.
*数量訂正ファイル
     SELECT   NFSUTEF   ASSIGN    TO        DA-01-VI-NFSUTEL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       STE-F01   STE-F05
                                            STE-F08   STE-F06
                                            STE-F07   STE-F09
                        FILE  STATUS   IS   STE-ST.
*箱数ファイル
     SELECT   NFHAKOF   ASSIGN    TO        DA-01-VI-NFHAKOL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HAK-F01   HAK-F05
                                            HAK-F08   HAK-F06
                                            HAK-F07
                        FILE STATUS    IS   HAK-ST.
*基本情報ファイル
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       NFJ-F01   NFJ-F05
                                            NFJ-F06   NFJ-F07
                                            NFJ-F08   NFJ-F09
                        FILE      STATUS    NFJ-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*本発基本情報ファイル
 FD  NHJOHOF.
     COPY        NHJOHOF     OF        XFDLIB
                 JOINING     JHO       PREFIX.
*本発売上伝票ファイル
 FD  NHTDENF.
     COPY        NHTDENF     OF        XFDLIB
                 JOINING     DEN       PREFIX.
*箱数ファイル
 FD  NFHAKOF.
     COPY        NFHAKOF     OF        XFDLIB
                 JOINING     HAK       PREFIX.
*数量訂正ファイル
 FD  NFSUTEF.
     COPY        NFSUTEF     OF        XFDLIB
                 JOINING     STE       PREFIX.
*基本情報ファイル
 FD  NFJOHOF.
     COPY        NFJOHOF     OF        XFDLIB
                 JOINING     NFJ       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  JHO-ST              PIC  X(02)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
     03  HAK-ST              PIC  X(02)  VALUE  SPACE.
     03  STE-ST              PIC  X(02)  VALUE  SPACE.
     03  NFJ-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  DEN-INV-FLG         PIC  X(03)  VALUE  SPACE.
     03  NFJ-INV-FLG         PIC  X(03)  VALUE  SPACE.
     03  DEN-DL-CNT          PIC  9(07)  VALUE  ZERO.
     03  JHO-DL-CNT          PIC  9(07)  VALUE  ZERO.
     03  HAK-DL-CNT          PIC  9(07)  VALUE  ZERO.
     03  STE-DL-CNT          PIC  9(07)  VALUE  ZERO.
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  JHO-ERR             PIC  N(10)  VALUE
                   NC"本発基本情報Ｆ異常".
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"本発売上伝票Ｆ異常".
     03  HAK-ERR             PIC  N(10)  VALUE
                   NC"箱数Ｆ異常".
     03  STE-ERR             PIC  N(10)  VALUE
                   NC"数量訂正Ｆ異常".
     03  NFJ-ERR             PIC  N(10)  VALUE
                   NC"基本情報Ｆ異常".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
*01  LINK-KEKA                   PIC  X(01).
*
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-BUMON                  PIC  X(04).
 01  LINK-TANCD                  PIC  X(02).
 01  LINK-KANRINO                PIC  9(08).
 01  LINK-KEKA                   PIC  X(01).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-BUMON
                                             LINK-TANCD
                                             LINK-KANRINO
                                             LINK-KEKA.
****************************************************************
 DECLARATIVES.
 JHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NHJOHOF.
     DISPLAY     JHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
*    ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NHTDENF.
     DISPLAY     DEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
*    ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 HAK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFHAKOF.
     DISPLAY     HAK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HAK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
*    ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 STE-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFSUTEF.
     DISPLAY     STE-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     STE-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
*    ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 NFJ-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFJOHOF.
     DISPLAY     NFJ-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     NFJ-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
*    ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        I-O         NHJOHOF  NHTDENF  NFHAKOF  NFSUTEF
                 INPUT       NFJOHOF.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
*管理番号が本発分かチェック
     IF  LINK-KANRINO(1:1) NOT = 9
         DISPLAY NC"＃指定管理番号　本発以外ＤＴ＃" UPON CONS
         MOVE    "END"         TO   END-FLG
         MOVE    "4"           TO   LINK-KEKA
         GO                    TO   INIT-EXIT
     END-IF.
*本発用基本情報ファイルＳＴＡＲＴ
     MOVE        SPACE            TO   JHO-REC.
     INITIALIZE  JHO-REC.
*
     MOVE        LINK-KANRINO     TO   JHO-F01.
     START  NHJOHOF KEY  IS  >=   JHO-F01  JHO-F05  JHO-F06
                                  JHO-F07  JHO-F08  JHO-F09
            INVALID
            DISPLAY NC"＃指定管理番号　存在無しＳＴ＃" UPON CONS
            MOVE    "END"         TO   END-FLG
            MOVE    "3"           TO   LINK-KEKA
            GO                    TO   INIT-EXIT
     END-START.
 INIT-010.
*本発用基本情報ファイル読込
     PERFORM  JHO-RD-SEC.
*対象データが存在しない場合
     IF  END-FLG = "END"
         DISPLAY NC"＃指定管理番号　存在無しＲＤ＃" UPON CONS
         MOVE    "END"         TO   END-FLG
         MOVE    "3"           TO   LINK-KEKA
         GO                    TO   INIT-EXIT
     END-IF.
*基本情報ファイルを読み状態確認
     PERFORM  NFJ-RD-SEC.
*対象データが存在しない場合
     IF  END-FLG = "END"
         DISPLAY NC"＃計上ＦＬＧ確認　エラー！！＃" UPON CONS
         GO                    TO   INIT-EXIT
     ELSE
*********箱数ファイルを連続削除
         PERFORM  HAK-DL-SEC
*********数量訂正ファイル連続削除
         PERFORM  STE-DL-SEC
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*本発売上伝票ファイル削除
     MOVE      JHO-F04       TO        DEN-F01.
     MOVE      JHO-F07       TO        DEN-F02.
     MOVE      ZERO          TO        DEN-F04.
     MOVE      40            TO        DEN-F051.
     MOVE      JHO-F06       TO        DEN-F07.
     MOVE      JHO-F09       TO        DEN-F112.
     MOVE      JHO-F08       TO        DEN-F03.
     PERFORM  DEN-RD-SEC.
*
     IF  DEN-INV-FLG = SPACE
         DELETE  NHTDENF
         ADD     1           TO        DEN-DL-CNT
     ELSE
         DISPLAY NC"＃本発売上伝票Ｆ　未存在！！＃" UPON CONS
     END-IF.
*本発用基本情報ファイル削除
     DELETE  NHJOHOF.
     ADD     1               TO        JHO-DL-CNT.
*本発用基本情報ファイル読込
     PERFORM  JHO-RD-SEC.
     MOVE    ZERO            TO        LINK-KEKA.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    本発用基本情報ファイル読込
****************************************************************
 JHO-RD-SEC                  SECTION.
     MOVE     "JHO-RD-SEC"        TO   S-NAME.
*
     READ     NHJOHOF   NEXT  AT  END
              MOVE      "END"     TO   END-FLG
              GO                  TO   JHO-RD-EXIT
     END-READ.
*管理番号チェック
     IF  JHO-F01  NOT =  LINK-KANRINO
              MOVE      "END"     TO   END-FLG
     END-IF.
*
 JHO-RD-EXIT.
     EXIT.
****************************************************************
*    基本情報ファイル読込
****************************************************************
 NFJ-RD-SEC                  SECTION.
     MOVE     "NFJ-RD-SEC"        TO   S-NAME.
*
     MOVE     JHO-F01             TO   NFJ-F01.
     MOVE     JHO-F05             TO   NFJ-F05.
     MOVE     JHO-F06             TO   NFJ-F06.
     MOVE     JHO-F07             TO   NFJ-F07.
     MOVE     JHO-F08             TO   NFJ-F08.
     MOVE     JHO-F09             TO   NFJ-F09.
     READ     NFJOHOF   INVALID
              MOVE      "INV"     TO   NFJ-INV-FLG
              GO                  TO   NFJ-RD-EXIT
              NOT  INVALID
              MOVE      SPACE     TO   NFJ-INV-FLG
     END-READ.
*発注確定済みＦＬＧチェック
     IF  NFJ-F34  =  "1"
         DISPLAY NC"＃指定管理番号　発注確定済！＃" UPON CONS
         MOVE    "END"            TO   END-FLG
         MOVE    "1"              TO   LINK-KEKA
*        GO                       TO   NFJ-RD-EXIT
     END-IF.
*ＴＲＡＮＴＲＡＮ取込済チェック
     IF  NFJ-F39  =  "1"
         DISPLAY NC"＃指定管理番号　ＴＲＮ計上済＃" UPON CONS
         MOVE    "END"            TO   END-FLG
         MOVE    "2"              TO   LINK-KEKA
         GO                       TO   NFJ-RD-EXIT
     END-IF.
*
 NFJ-RD-EXIT.
     EXIT.
****************************************************************
*     箱数ファイル削除
****************************************************************
 HAK-DL-SEC                  SECTION.
     MOVE     "HAK-DL-SEC"        TO   S-NAME.
*
     MOVE     SPACE          TO   HAK-REC.
     INITIALIZE                   HAK-REC.
*
     MOVE     LINK-KANRINO   TO   HAK-F01.
     START  NFHAKOF  KEY  IS  >=  HAK-F01  HAK-F05  HAK-F08
                                  HAK-F06  HAK-F07
            INVALID
            DISPLAY NC"＃箱数ファイル　削除箱Ｆ無！＃" UPON CONS
            GO               TO   HAK-DL-EXIT
     END-START.
*
 HAK010.
     READ  NFHAKOF
           NEXT  AT  END
           GO                TO   HAK-DL-EXIT
     END-READ.
*
     IF  HAK-F01  NOT =  LINK-KANRINO
         GO                  TO   HAK-DL-EXIT
     END-IF.
*
     DELETE  NFHAKOF.
     ADD     1               TO   HAK-DL-CNT.
*
     GO                      TO   HAK010.
*
 HAK-DL-EXIT.
     EXIT.
****************************************************************
*     数量訂正ファイル削除
****************************************************************
 STE-DL-SEC                  SECTION.
     MOVE     "STE-DL-SEC"        TO   S-NAME.
*
     MOVE     SPACE          TO   STE-REC.
     INITIALIZE                   STE-REC.
*
     MOVE     LINK-KANRINO   TO   STE-F01.
     START  NFSUTEF  KEY  IS  >=  STE-F01  STE-F05  STE-F08
                                  STE-F06  STE-F07  STE-F09
            INVALID
            DISPLAY NC"＃数量訂正Ｆ　　削除対象無！＃" UPON CONS
            GO               TO   STE-DL-EXIT
     END-START.
*
 STE010.
     READ  NFSUTEF
           NEXT  AT  END
           GO                TO   STE-DL-EXIT
     END-READ.
*
     IF  STE-F01  NOT =  LINK-KANRINO
         GO                  TO   STE-DL-EXIT
     END-IF.
*
     DELETE  NFSUTEF.
     ADD     1               TO   STE-DL-CNT.
*
     GO                      TO   STE010.
*
 STE-DL-EXIT.
     EXIT.
****************************************************************
*    本発売上伝票ファイル読込
****************************************************************
 DEN-RD-SEC             SECTION.
*
     MOVE     "DEN-RD-SEC"        TO   S-NAME.
*
     READ    NHTDENF
             INVALID
             MOVE   "INV"         TO   DEN-INV-FLG
             NOT  INVALID
             MOVE   SPACE         TO   DEN-INV-FLG
     END-READ.
*
 DEN-RD-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     DISPLAY NC"＃削除管理番号" " = " LINK-KANRINO    UPON CONS.
     DISPLAY NC"＃本発基本ＤＬ" " = " JHO-DL-CNT      UPON CONS.
     DISPLAY NC"＃本発売上ＤＬ" " = " DEN-DL-CNT      UPON CONS.
     DISPLAY NC"＃箱数ＤＬ　　" " = " HAK-DL-CNT      UPON CONS.
     DISPLAY NC"＃数量訂正ＤＬ" " = " STE-DL-CNT      UPON CONS.
*    DISPLAY "LINK-KEKA = " LINK-KEKA UPON CONS.
*
     CLOSE  NHJOHOF  NHTDENF  NFSUTEF  NFHAKOF  NFJOHOF.
*
 END-EXIT.
     EXIT.
