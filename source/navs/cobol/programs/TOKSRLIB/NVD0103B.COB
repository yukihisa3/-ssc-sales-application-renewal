****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　入荷予定機能　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　入荷予定機能　　　　*
*    モジュール名　　　　：　重複分再更新　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/06/14 NAV TAKAHASHI          *
*    処理概要　　　　　　：　入荷予定チェックデータを読み、　  *
*                            重複分の再更新を行なう。　　　　。*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVD0103B.
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
*入荷予定チェックデータ
     SELECT   YTICHKF   ASSIGN    TO        DA-01-YTICHKF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   CHK-ST.
*倉庫別商品別_番設定マスタ
     SELECT   SOKTANF   ASSIGN    TO        DA-01-VI-SOKTANL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01   TAN-F02
                        FILE STATUS    IS   TAN-ST.
*発注明細ファイル
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F02  MEI-F03
                        FILE STATUS    IS   MEI-ST.
*入荷予定累積ファイル
     SELECT   NYKRUIF   ASSIGN    TO        DA-01-VI-NYKRUIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUI-F01  RUI-F02
                                            RUI-F03
                        FILE STATUS    IS   RUI-ST.
*商品在庫マスタ
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01
                                            ZAI-F02
                                            ZAI-F03
                        FILE STATUS    IS   ZAI-ST.
*ＳＵＢ商品名称マスタ
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL7
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SBM-D01
                        FILE STATUS    IS   MEI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*入荷予定チェックデータ
 FD  YTICHKF
     LABEL       RECORD      IS        STANDARD.
     COPY        YTICHKF     OF        XFDLIB
     JOINING     CHK         AS        PREFIX.
*倉庫別商品別_番設定マスタ
 FD  SOKTANF
                        LABEL RECORD   IS   STANDARD.
     COPY     SOKTANF   OF        XFDLIB
              JOINING   TAN  AS   PREFIX.
*発注明細ファイル
 FD  HACMEIF
                        LABEL RECORD   IS   STANDARD.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   MEI  AS   PREFIX.
*入荷予定累積ファイル
 FD  NYKRUIF
                        LABEL RECORD   IS   STANDARD.
     COPY     NYKRUIF   OF        XFDLIB
              JOINING   RUI  AS   PREFIX.
*商品在庫マスタ
 FD  ZAMZAIF
                        LABEL RECORD   IS   STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI  AS   PREFIX.
*ＳＵＢ商品名称マスタ
 FD  SUBMEIF
                        LABEL RECORD   IS   STANDARD.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SBM  AS   PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CHK-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  RUI-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  SBM-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  INV-SW              PIC  9(01)  VALUE  ZERO.
     03  WK-RD-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-TS-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-MR-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-RR-CNT           PIC  9(09)  VALUE  ZERO.
     03  SOKTANF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  ZAMZAIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HACMEIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  NYKRUIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  SUBMEIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
*
 01  WK-CHK-F11              PIC S9(10)  VALUE  ZERO.
*ブレイクキー
 01  WRK-KEY.
     03  WRK-F09             PIC  X(20)  VALUE  ZERO.
     03  WRK-F10             PIC  9(02)  VALUE  ZERO.
*
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
*
 01  FILE-ERR.
     03  CHK-ERR             PIC  N(10)  VALUE
                   NC"入荷予定ＣＨＫ　異常".
     03  TAN-ERR             PIC  N(10)  VALUE
                   NC"倉庫商品_番Ｍ　異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"発注明細Ｆ　　　異常".
     03  RUI-ERR             PIC  N(10)  VALUE
                   NC"入荷予定累積Ｆ　異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ　　　異常".
     03  SBM-ERR             PIC  N(10)  VALUE
                   NC"ＳＵＢ商品名称Ｍ異常".
*入荷予定情報格納
     COPY   YTINYKF  OF XFDLIB  JOINING   YNY  AS   PREFIX.
*ＮＡＶＳ伝票番号変換
 01  WK-NAVS-DENNO-AREA.
     03  WK-NAVS-DENNO       PIC X(10).
     03  FILLER              REDEFINES  WK-NAVS-DENNO.
         05  WK-NAVS-DENNO-H PIC 9(10).
*日付変換
 01  HIDUKE-HENKAN.
     03  WK-HIDUKE-HEN.
         05  WK-HIDUKE-YYYY  PIC X(04).
         05  WK-HIDUKE-KU1   PIC X(01).
         05  WK-HIDUKE-MM    PIC X(02).
         05  WK-HIDUKE-KU2   PIC X(01).
         05  WK-HIDUKE-DD    PIC X(02).
     03  WK-HIDUKE-CHG       PIC X(08).
     03  FILLER              REDEFINES  WK-HIDUKE-CHG.
         05  WK-HIDUKE-CHG-R PIC 9(08).
 01  WK-JYO-F04              PIC 9(07)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 LINKAGE                     SECTION.
 01  PARA-SIJINO             PIC 9(10).
****************************************************************
 PROCEDURE                   DIVISION  USING  PARA-SIJINO.
****************************************************************
 DECLARATIVES.
 CHK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE YTICHKF.
     DISPLAY     CHK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CHK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SOKTANF.
     DISPLAY     TAN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TAN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HACMEIF.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 RUI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NYKRUIF.
     DISPLAY     RUI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     RUI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SBM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIF.
     DISPLAY     SBM-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SBM-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
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
     OPEN        I-O         HACMEIF  ZAMZAIF  NYKRUIF
                 INPUT       YTICHKF  SOKTANF  SUBMEIF.
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
 INIT-010.
*入荷予定チェックデータ読込
     PERFORM  CHK-RD-SEC.
*
     IF  END-FLG  =  "END"
         DISPLAY NC"＃重複対象データ無！！＃"  UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
*    発注明細読込
     MOVE      CHK-F98            TO   WK-NAVS-DENNO.
     MOVE      WK-NAVS-DENNO-H    TO   MEI-F02.
     MOVE      CHK-F99            TO   MEI-F03.
     PERFORM   HACMEIF-READ-SEC.
*    入荷累積読込
     MOVE      CHK-F98            TO   RUI-F060.
     MOVE      CHK-F99            TO   RUI-F061.
     PERFORM   NYKRUIF-READ-SEC.
*サカタ商品ＣＤ取得
     MOVE   CHK-F08          TO   SBM-D01.
     PERFORM  SUBMEIF-READ-SEC.
     IF  SUBMEIF-INV-FLG = "INV"
         DISPLAY NC"＃ＳＵＢ商品　未登録エラー！＃" UPON CONS
         DISPLAY "## MEI-F02 = " MEI-F02            UPON CONS
         DISPLAY "## MEI-F03 = " MEI-F03            UPON CONS
         DISPLAY "## CHK-F08 = " CHK-F08            UPON CONS
         MOVE     4000       TO   PROGRAM-STATUS
         STOP  RUN
     END-IF.
*更新判定（発注明細）
     IF  HACMEIF-INV-FLG  =  SPACE
         IF   MEI-F05  =  ZERO
         AND  MEI-F10  =  ZERO
              COMPUTE  WK-CHK-F11  =  CHK-F11  /  100
              MOVE     WK-CHK-F11 TO   MEI-F09
              MOVE     WK-DATE8   TO   MEI-F99
              REWRITE  MEI-REC
              ADD      1           TO  WK-MR-CNT
**************更新判定（入荷累積）
              IF  NYKRUIF-INV-FLG  =  SPACE
                  MOVE  CHK-REC            TO   RUI-F06
                  REWRITE  RUI-REC
                  ADD      1                TO  WK-RR-CNT
              END-IF
         END-IF
     END-IF.
*
 MEI010.
     PERFORM  CHK-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    新入出庫ファイル読込　　　　　　　　
****************************************************************
 CHK-RD-SEC                 SECTION.
     MOVE     "CHK-RD-SEC"        TO   S-NAME.
*
     READ     YTICHKF   AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   CHK-RD-EXIT
              NOT  AT  END
              ADD       1         TO   WK-RD-CNT
     END-READ.
*
 DND010.
     IF  WK-RD-CNT(7:3)  =  "000"  OR  "500"
         DISPLAY "## READ-CNT = " WK-RD-CNT UPON CONS
     END-IF.
 DND020.
*対象チェック
     IF  CHK-F98  NOT =  SPACE
         CONTINUE
     ELSE
         GO                       TO   CHK-RD-SEC
     END-IF.
*
 CHK-RD-EXIT.
     EXIT.
****************************************************************
*               発注明細ファイル作成
****************************************************************
 HACMEIF-READ-SEC            SECTION.
     MOVE     "HACMEIF-READ-SEC"  TO   S-NAME.
*
     READ  HACMEIF
           INVALID     MOVE  "INV"     TO   HACMEIF-INV-FLG
           NOT INVALID MOVE  SPACE     TO   HACMEIF-INV-FLG
     END-READ.
*
 HACMEIF-READ-EXIT.
     EXIT.
****************************************************************
*               入荷予定累積ファイル作成
****************************************************************
 NYKRUIF-READ-SEC            SECTION.
     MOVE     "NYKRUIF-READ-SEC"  TO   S-NAME.
*存在チェック
     MOVE     CHK-F98        TO   RUI-F01
     MOVE     CHK-F01        TO   RUI-F02.
     MOVE     CHK-F99        TO   RUI-F03.
     READ  NYKRUIF
           INVALID     MOVE  "INV"     TO   NYKRUIF-INV-FLG
           NOT INVALID MOVE  SPACE     TO   NYKRUIF-INV-FLG
     END-READ.
*
 NYKRUIF-READ-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    CHK-F15(2:2)    TO   ZAI-F01.
*****特別処理
     IF  CHK-F15  =  "106"
         MOVE "01"           TO   ZAI-F01
     END-IF.
     IF  CHK-F15  =  "107"
         MOVE "41"           TO   ZAI-F01
     END-IF.
     IF  CHK-F15  =  "149"
         MOVE "49"           TO   ZAI-F01
     END-IF.
     IF  CHK-F15  =  "1T1"
         MOVE "TI"           TO   ZAI-F01
     END-IF.
     MOVE    SBM-F011        TO   ZAI-F021.
     MOVE    SBM-F012        TO   ZAI-F022.
     MOVE    MEI-F08         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      CHK-F15(2:2)  TO   ZAI-F01.
*****特別処理
     IF  CHK-F15  =  "106"
         MOVE "01"           TO   ZAI-F01
     END-IF.
     IF  CHK-F15  =  "107"
         MOVE "41"           TO   ZAI-F01
     END-IF.
     IF  CHK-F15  =  "149"
         MOVE "49"           TO   ZAI-F01
     END-IF.
     IF  CHK-F15  =  "1T1"
         MOVE "TI"           TO   ZAI-F01
     END-IF.
      MOVE      SBM-F011      TO   ZAI-F021.
      MOVE      SBM-F012      TO   ZAI-F022.
      MOVE      MEI-F08       TO   ZAI-F03.
*未入庫数加算
      COMPUTE   ZAI-F26       =    ZAI-F26  +  WK-CHK-F11.
*商品名称マスタ読込み
      PERFORM   SUBMEIF-READ-SEC.
*商品名称マスタ存在チェック
      IF  SUBMEIF-INV-FLG  =  SPACE
          MOVE  SBM-F031      TO   ZAI-F30
          MOVE  WK-DATE8      TO   ZAI-F98
          MOVE  WK-DATE8      TO   ZAI-F99
          WRITE ZAI-REC
      END-IF.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
*    未入庫数加算
     COMPUTE   ZAI-F26       =    ZAI-F26  +  WK-CHK-F11.
     MOVE     WK-DATE8  TO   ZAI-F99.
*    商品在庫マスタ更新
     REWRITE  ZAI-REC.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*              ＳＵＢ商品名称マスタ読込                        *
****************************************************************
 SUBMEIF-READ-SEC          SECTION.
*
     MOVE      "SUBMEIF-READ-SEC" TO    S-NAME.
*
     READ      SUBMEIF
               INVALID
               MOVE      "INV"    TO    SUBMEIF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SUBMEIF-INV-FLG
     END-READ.
*
 SUBMEIF-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     DISPLAY "READ-CNT  = "  WK-RD-CNT  UPON CONS.
     DISPLAY "MEI-CNT   = "  WK-MR-CNT  UPON CONS.
     DISPLAY "RUI-CNT   = "  WK-RR-CNT  UPON CONS.
*
     CLOSE       HACMEIF  ZAMZAIF  NYKRUIF
                 YTICHKF  SOKTANF  SUBMEIF.
 END-EXIT.
     EXIT.
