****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　入荷予定機能　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　入荷予定機能　　　　*
*    モジュール名　　　　：　発注Ｆ更新処理（入庫＆作業）　　　*
*    作成日／作成者　　　：　2020/05/08 TAKAHASHI              *
*    処理概要　　　　　　：　ＮＡＶＳ受信データ管理Ｆを読み、  *
*                            発注Ｈ，Ｍ、作業、累積を作成する。*
*　　更新日／更新者　　　：　2020/10/05 NAV TAKAHASHI          *
*    修正概要　　　　　　：　伝区設定対応　　　　　　　        *
*　　更新日／更新者　　　：　2020/11/19 NAV TAKAHASHI          *
*    修正概要　　　　　　：　作業実績データ作成を停止　        *
*　　更新日／更新者　　　：　2021/06/08 NAV TAKAHASHI          *
*    修正概要　　　　　　：　発注時、行番号を１固定にする。    *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVD0140B.
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
*ＮＡＶＳ受信データ管理ファイル
     SELECT   DNDTKNF   ASSIGN    TO        DA-01-VI-DNDTKNL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DND-F01   DND-F07
                                            DND-F08   DND-F09
                                            DND-F10
                        FILE  STATUS   IS   DND-ST.
*倉庫別商品別_番設定マスタ
     SELECT   SOKTANF   ASSIGN    TO        DA-01-VI-SOKTANL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01   TAN-F02
                        FILE STATUS    IS   TAN-ST.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE STATUS    IS   JYO-ST.
*ＳＵＢ商品名称マスタ
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL7
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SBM-D01
                        FILE STATUS    IS   MEI-ST.
*発注ヘッダファイル
     SELECT   HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HED-F02
                        FILE STATUS    IS   HED-ST.
*発注明細ファイル
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F02  MEI-F03
                        FILE STATUS    IS   MEI-ST.
*作業実績ファイル
     SELECT   SGYFILF   ASSIGN    TO        DA-01-VI-SGYFILL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SGY-F01  SGY-F03
                        FILE STATUS    IS   SGY-ST.
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
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*ＮＡＶＳ受信データ管理ファイル
 FD  DNDTKNF
                        LABEL RECORD   IS   STANDARD.
     COPY     DNDTKNF   OF        XFDLIB
              JOINING   DND  AS   PREFIX.
*倉庫別商品別_番設定マスタ
 FD  SOKTANF
                        LABEL RECORD   IS   STANDARD.
     COPY     SOKTANF   OF        XFDLIB
              JOINING   TAN  AS   PREFIX.
*条件ファイル
 FD  HJYOKEN
                        LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO  AS   PREFIX.
*ＳＵＢ商品名称マスタ
 FD  SUBMEIF
                        LABEL RECORD   IS   STANDARD.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SBM  AS   PREFIX.
*発注ヘッダファイル
 FD  HACHEDF
                        LABEL RECORD   IS   STANDARD.
     COPY     HACHEDF   OF        XFDLIB
              JOINING   HED  AS   PREFIX.
*発注明細ファイル
 FD  HACMEIF
                        LABEL RECORD   IS   STANDARD.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   MEI  AS   PREFIX.
*作業事績ファイル
 FD  SGYFILF
                        LABEL RECORD   IS   STANDARD.
     COPY     SGYFILF   OF        XFDLIB
              JOINING   SGY  AS   PREFIX.
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
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  DND-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  SBM-ST              PIC  X(02)  VALUE  SPACE.
     03  HED-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  SGY-ST              PIC  X(02)  VALUE  SPACE.
     03  RUI-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  INV-SW              PIC  9(01)  VALUE  ZERO.
     03  WK-RD-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-HD-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-ME-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-SG-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-RU-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-ER-CNT           PIC  9(09)  VALUE  ZERO.
     03  HJYOKEN-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  SOKTANF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  SUBMEIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  ZAMZAIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HACHEDF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HACMEIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  SGYFILF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  NYKRUIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
*
 01  WK-YNY-F11              PIC S9(10)  VALUE  ZERO.
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
     03  DND-ERR             PIC  N(10)  VALUE
                   NC"ＮＡＶＳ受信管理異常".
     03  TAN-ERR             PIC  N(10)  VALUE
                   NC"倉庫商品_番Ｍ　異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件Ｆ　　　　　異常".
     03  SBM-ERR             PIC  N(10)  VALUE
                   NC"ＳＵＢ商品名称Ｍ異常".
     03  HED-ERR             PIC  N(10)  VALUE
                   NC"発注ヘッダＦ　　異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"発注明細Ｆ　　　異常".
     03  SGY-ERR             PIC  N(10)  VALUE
                   NC"作業実績Ｆ　　　異常".
     03  RUI-ERR             PIC  N(10)  VALUE
                   NC"入荷予定累積Ｆ　異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ　　　異常".
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
 DND-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DNDTKNF.
     DISPLAY     DND-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DND-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SOKTANF.
     DISPLAY     TAN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TAN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SBM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIF.
     DISPLAY     SBM-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SBM-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 HED-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HACHEDF.
     DISPLAY     HED-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HED-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HACMEIF.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SGY-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SGYFILF.
     DISPLAY     SGY-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SGY-ST      UPON      CONS.
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
     OPEN        I-O         HACHEDF  HACMEIF  ZAMZAIF  DNDTKNF
                             SGYFILF  NYKRUIF
                 INPUT       SUBMEIF  SOKTANF.
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
*ＮＡＶＳ受信データ管理ファイルスタート
     MOVE        SPACE       TO   DND-REC.
     INITIALIZE  DND-REC.
     MOVE      PARA-SIJINO   TO   DND-F01.
     START DNDTKNF  KEY  IS  >=  DND-F01  DND-F07  DND-F08
                                 DND-F09  DND-F10
             INVALID
             MOVE  "END"           TO   END-FLG
             DISPLAY NC"＃対象ＤＴ無し（ＳＴ）＃"  UPON  CONS
             GO                    TO   INIT-EXIT
     END-START.
 INIT-010.
*ＮＡＶＳ受信データ管理ファイル読込
     PERFORM  DND-RD-SEC.
*
     IF  END-FLG  =  "END"
         DISPLAY NC"＃対象ＤＴ無し（初回ＲＤ）＃"  UPON CONS
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
*****DISPLAY "DND-F09 = " DND-F09 "/DND-F10 = "DND-F10 UPON CONS.
*****DISPLAY "WRK-F09 = " WRK-F09 "/WRK-F10 = "WRK-F10 UPON CONS.
*#2021/06/08 NAV ST
*****IF   DND-F09  =  WRK-F09
     IF   DND-F11  =  WRK-F09(1:10)
*#2021/06/08 NAV ED
*****AND  DND-F10  =  WRK-F10
          CONTINUE
     ELSE
**********発注ヘッダファイル作成
          PERFORM  HACHEDF-WT-SEC
*#2021/06/08 NAV ST
***       MOVE   DND-F09          TO   WRK-F09
          MOVE   SPACE            TO   WRK-F09
          MOVE   DND-F11          TO   WRK-F09(1:10)
*#2021/06/08 NAV ED
**********MOVE   DND-F10          TO   WRK-F10
     END-IF.
*発注明細データ作成
     PERFORM  HACMEIF-WT-SEC.
*作業実績Ｆ作成チェック
*****IF  YNY-F09  NOT =  SPACE
*****    PERFORM SGYFILF-WT-SEC
*****END-IF.
*入荷予定累積ファイル作成
*****PERFORM  NYKRUIF-WT-SEC.
*
 MEI010.
     PERFORM  DND-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    新入出庫ファイル読込　　　　　　　　
****************************************************************
 DND-RD-SEC                 SECTION.
     MOVE     "DND-RD-SEC"        TO   S-NAME.
*
     READ     DNDTKNF   AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   DND-RD-EXIT
              NOT  AT  END
              ADD       1         TO   WK-RD-CNT
     END-READ.
*
 DND010.
     IF  WK-RD-CNT(7:3)  =  "000"  OR  "500"
         DISPLAY "## READ-CNT = " WK-RD-CNT UPON CONS
     END-IF.
 DND020.
*送受信指示ＮＯチェック
*****DISPLAY "PARA-SIJINO = " PARA-SIJINO  UPON CONS.
*****DISPLAY "DND-F01     = " DND-F01      UPON CONS.
     IF  PARA-SIJINO  =  DND-F01
         CONTINUE
     ELSE
         MOVE  "END"              TO   END-FLG
         GO                       TO   DND-RD-EXIT
     END-IF.
 DND030.
*対象判定（エラー区分＝空白が対象）
     IF  DND-F21  =  SPACE
         CONTINUE
     ELSE
         GO                       TO   DND-RD-SEC
     END-IF.
 DND040.
*入荷予定データワークセット
     MOVE       DND-F20           TO   YNY-REC.
*
 DND-RD-EXIT.
     EXIT.
****************************************************************
*               発注ヘッダファイル作成
****************************************************************
 HACHEDF-WT-SEC              SECTION.
     MOVE     "HACHEDF-WT-SEC"    TO   S-NAME.
*
     MOVE     DND-F11        TO   WK-NAVS-DENNO.
     MOVE     WK-NAVS-DENNO-H TO  HED-F02.
*****DISPLAY "HED-F02 =  " HED-F02  UPON CONS.
     READ  HACHEDF
           INVALID     MOVE  "INV"     TO   HACHEDF-INV-FLG
           NOT INVALID MOVE  SPACE     TO   HACHEDF-INV-FLG
     END-READ.
*
     IF    HACHEDF-INV-FLG  =  SPACE
           DISPLAY NC"＃同一キーが存在します！＃" UPON CONS
           DISPLAY NC"＃伝票番号" " = " HED-F02   UPON CONS
           GO                TO   HACHEDF-WT-EXIT
     END-IF.
*
     MOVE     SPACE          TO   HED-REC.
     INITIALIZE                   HED-REC.
*伝票区分
*#2020/10/05 NAV ST　符号区分="-"で数量が０以外の場合、５１SET
*****IF  YNY-F10  =  "-"
     IF  YNY-F10  =  "-" AND YNY-F11 NOT =  ZERO
*#2020/10/05 NAV ED
         MOVE 51             TO   HED-F01
     ELSE
         MOVE 50             TO   HED-F01
     END-IF.
*伝票番号
     MOVE     WK-NAVS-DENNO-H TO  HED-F02.
*仕入先
     MOVE     YNY-F03        TO   HED-F06.
*担当者
     MOVE     99             TO   HED-F05.
*発注日
     MOVE     YNY-F04        TO   WK-HIDUKE-HEN.
     MOVE     WK-HIDUKE-YYYY TO   WK-HIDUKE-CHG(1:4).
     MOVE     WK-HIDUKE-MM   TO   WK-HIDUKE-CHG(5:2).
     MOVE     WK-HIDUKE-DD   TO   WK-HIDUKE-CHG(7:2).
     MOVE     WK-HIDUKE-CHG-R TO  HED-F10.
*納入予定日
     MOVE     YNY-F05        TO   WK-HIDUKE-HEN.
     MOVE     WK-HIDUKE-YYYY TO   WK-HIDUKE-CHG(1:4).
     MOVE     WK-HIDUKE-MM   TO   WK-HIDUKE-CHG(5:2).
     MOVE     WK-HIDUKE-DD   TO   WK-HIDUKE-CHG(7:2).
     MOVE     WK-HIDUKE-CHG-R TO  HED-F11.
*税区分
     MOVE     0              TO   HED-F13.
*倉庫ＣＤ
     MOVE     YNY-F15(2:2)   TO   HED-F17.
*****特別処理
     IF  YNY-F15  =  "106"
         MOVE "01"           TO   HED-F17
     END-IF.
     IF  YNY-F15  =  "107"
         MOVE "41"           TO   HED-F17
     END-IF.
     IF  YNY-F15  =  "149"
         MOVE "49"           TO   HED-F17
     END-IF.
     IF  YNY-F15  =  "1T1"
         MOVE "TI"           TO   HED-F17
     END-IF.
*発注区分／発注書発行区分
     MOVE     1              TO   HED-F25  HED-F97.
*データ種別
     MOVE     DND-F04        TO   HED-F311.
*Ｄ３６５伝票番号
     MOVE     DND-F09        TO   HED-F313.
*登録日
     MOVE     WK-DATE8       TO   HED-F98.
*更新日
     MOVE     WK-DATE8       TO   HED-F99.
*ヘッダ作成
*T
**   DISPLAY "HED-F02=" HED-F02 UPON CONS.
*T
     WRITE  HED-REC.
     ADD      1              TO   WK-HD-CNT.
*
 HACHEDF-WT-EXIT.
     EXIT.
****************************************************************
*               発注明細ファイル作成
****************************************************************
 HACMEIF-WT-SEC              SECTION.
     MOVE     "HACMEIF-WT-SEC"    TO   S-NAME.
*
     MOVE     WK-NAVS-DENNO-H TO  MEI-F02.
*#2021/06/08 NAV ST
*****MOVE     YNY-F02        TO   MEI-F03.
     MOVE     1              TO   MEI-F03.
*#2021/06/08 NAV ED
**   DISPLAY "MEI-F02 = " MEI-F02 " MEI-F03 = " MEI-F03 UPON CONS.
     READ  HACMEIF
           INVALID     MOVE  "INV"     TO   HACMEIF-INV-FLG
           NOT INVALID MOVE  SPACE     TO   HACMEIF-INV-FLG
     END-READ.
*
     IF    HACMEIF-INV-FLG  =  SPACE
           DISPLAY NC"＃同一キーが存在します！＃" UPON CONS
           DISPLAY NC"＃伝票番号" " = " MEI-F02   UPON CONS
           DISPLAY NC"＃行番号　" " = " MEI-F03   UPON CONS
           GO                TO   HACMEIF-WT-EXIT
     END-IF.
*
     MOVE     SPACE          TO   MEI-REC.
     INITIALIZE                   MEI-REC.
*伝票区分
*****IF  YNY-F10  =  "-"
*********MOVE 51             TO   MEI-F01
*****ELSE
*********MOVE 50             TO   MEI-F01
*****END-IF.
*伝票区分
*#2020/10/05 NAV ST　符号区分="-"で数量が０以外の場合、５１SET
*****IF  YNY-F10  =  "-"
     IF  YNY-F10  =  "-" AND YNY-F11 NOT =  ZERO
*#2020/10/05 NAV ED
         MOVE 51             TO   MEI-F01
     ELSE
         MOVE 50             TO   MEI-F01
     END-IF.
*伝票番号
     MOVE   WK-NAVS-DENNO-H  TO   MEI-F02.
*行番号
*#2021/06/08 NAV ST
*****MOVE   YNY-F02          TO   MEI-F03.
     MOVE     1              TO   MEI-F03.
*#2021/06/08 NAV ED
*サカタ商品ＣＤ取得
     MOVE   YNY-F08          TO   SBM-D01.
     PERFORM  SUBMEIF-READ-SEC.
     IF  SUBMEIF-INV-FLG = "INV"
         DISPLAY NC"＃ＳＵＢ商品　未登録エラー！＃" UPON CONS
         DISPLAY "## MEI-F02 = " MEI-F02            UPON CONS
         DISPLAY "## MEI-F03 = " MEI-F03            UPON CONS
         DISPLAY "## YNY-F08 = " YNY-F08            UPON CONS
         MOVE     4000       TO   PROGRAM-STATUS
         STOP  RUN
     END-IF.
*商品ＣＤ
     MOVE     SBM-F011       TO   MEI-F06.
*品単ＣＤ　
     MOVE     SBM-F0121      TO   MEI-F07(1:5).
     MOVE     SBM-F0122      TO   MEI-F07(6:2).
     MOVE     SBM-F0123      TO   MEI-F07(8:1).
*_番（倉庫別商品別_番設定Ｍ索引）
     MOVE     YNY-F15(2:2)   TO   TAN-F01.
*****特別処理
     IF  YNY-F15  =  "106"
         MOVE "01"           TO   TAN-F01
     END-IF.
     IF  YNY-F15  =  "107"
         MOVE "41"           TO   TAN-F01
     END-IF.
     IF  YNY-F15  =  "149"
         MOVE "49"           TO   TAN-F01
     END-IF.
     IF  YNY-F15  =  "1T1"
         MOVE "TI"           TO   TAN-F01
     END-IF.
     MOVE     YNY-F08        TO   TAN-F02.
     PERFORM  SOKTANF-READ-SEC.
     IF   SOKTANF-INV-FLG =  "INV"
          MOVE  SPACE        TO   MEI-F08
     ELSE
          IF  TAN-F06  >  ZERO
          AND TAN-F06  <  WK-DATE8
              MOVE  TAN-F07   TO   MEI-F08
          ELSE
              MOVE  TAN-F05   TO   MEI-F08
          END-IF
     END-IF.
*発注数
*#2020/11/26 NAV ST
*****MOVE     YNY-F11        TO   MEI-F09.
     COMPUTE  WK-YNY-F11  =  YNY-F11  /  100.
     MOVE     WK-YNY-F11     TO   MEI-F09.
*#2020/11/26 NAV ED
*単価区分
     MOVE     1              TO   MEI-F11.
*発注日
     MOVE     YNY-F04        TO   WK-HIDUKE-HEN.
     MOVE     WK-HIDUKE-YYYY TO   WK-HIDUKE-CHG(1:4).
     MOVE     WK-HIDUKE-MM   TO   WK-HIDUKE-CHG(5:2).
     MOVE     WK-HIDUKE-DD   TO   WK-HIDUKE-CHG(7:2).
     MOVE     WK-HIDUKE-CHG-R TO  MEI-F19.
*納入予定日
     MOVE     YNY-F05        TO   WK-HIDUKE-HEN.
     MOVE     WK-HIDUKE-YYYY TO   WK-HIDUKE-CHG(1:4).
     MOVE     WK-HIDUKE-MM   TO   WK-HIDUKE-CHG(5:2).
     MOVE     WK-HIDUKE-DD   TO   WK-HIDUKE-CHG(7:2).
     MOVE     WK-HIDUKE-CHG-R TO  MEI-F17.
*データ種別
     MOVE     DND-F04        TO   MEI-F22.
*Ｄ３６５伝票番号
     MOVE     DND-F09        TO   MEI-F21.
*登録日
     MOVE     WK-DATE8       TO   MEI-F98.
*更新日
     MOVE     WK-DATE8       TO   MEI-F99.
*在庫更新
*#2020/11/26 NAV ST
*****PERFORM  ZAIKO-SEC.
     IF   MEI-F01  NOT =  51
          PERFORM  ZAIKO-SEC
     END-IF.
*#2020/11/26 NAV ED
*明細作成
     WRITE  MEI-REC.
     ADD      1              TO   WK-ME-CNT.
*作業実績Ｆ作成チェック　更新場所を変更
*#2020/11/19 NAV ST 作業実績データ作成を停止
*****IF  YNY-F09  NOT =  SPACE
*        PERFORM SGYFILF-WT-SEC
*****END-IF.
*#2020/11/19 NAV ED 作業実績データ作成を停止
*入荷予定累積ファイル作成
     PERFORM  NYKRUIF-WT-SEC.
*
 HACMEIF-WT-EXIT.
     EXIT.
****************************************************************
*               作業実績ファイル作成
****************************************************************
 SGYFILF-WT-SEC              SECTION.
     MOVE     "SGYFILF-WT-SEC"    TO   S-NAME.
*作業ＮＯ採番
     PERFORM  SGYNO-SAIBAN-SEC.
*初期化（出庫レコード作成）
     MOVE     SPACE         TO   SGY-REC.
     INITIALIZE                  SGY-REC.
*作業ＮＯ
     MOVE     WK-JYO-F04    TO   SGY-F01.
*行ＮＯ
     MOVE     1             TO   SGY-F02.
*作業区分
     MOVE     58            TO   SGY-F03.
*作業場所
     MOVE     YNY-F15(2:2)  TO   SGY-F04.
*****特別処理
     IF  YNY-F15  =  "106"
         MOVE "01"           TO   SGY-F04
     END-IF.
     IF  YNY-F15  =  "107"
         MOVE "41"           TO   SGY-F04
     END-IF.
     IF  YNY-F15  =  "149"
         MOVE "49"           TO   SGY-F04
     END-IF.
     IF  YNY-F15  =  "1T1"
         MOVE "TI"           TO   SGY-F04
     END-IF.
*作業日
     MOVE     WK-DATE8      TO   SGY-F05.
*入出庫区分
     MOVE     "2"           TO   SGY-F06.
*ストックＮＯ
     MOVE     YNY-F09       TO   SGY-F07.
*商品ＣＤ
     MOVE     MEI-F06       TO   SGY-F08.
*品単ＣＤ
     MOVE     MEI-F07       TO   SGY-F09.
*_番
     MOVE     MEI-F08       TO   SGY-F10.
*数量　　　
     MOVE     MEI-F09       TO   SGY-F11.
*データ種別
     MOVE     DND-F04       TO   SGY-F89.
*登録日
     MOVE     WK-DATE8      TO   SGY-F98.
*修正日
     MOVE     WK-DATE8      TO   SGY-F99.
     WRITE    SGY-REC.
     ADD      1             TO   WK-SG-CNT.
*初期化（入庫レコード作成）
     MOVE     SPACE         TO   SGY-REC.
     INITIALIZE                  SGY-REC.
*作業ＮＯ
     MOVE     WK-JYO-F04    TO   SGY-F01.
*行ＮＯ
     MOVE     2             TO   SGY-F02.
*作業区分
     MOVE     58            TO   SGY-F03.
*作業場所
     MOVE     YNY-F15(2:2)  TO   SGY-F04.
*****特別処理
     IF  YNY-F15  =  "106"
         MOVE "01"           TO   SGY-F04
     END-IF.
     IF  YNY-F15  =  "107"
         MOVE "41"           TO   SGY-F04
     END-IF.
     IF  YNY-F15  =  "149"
         MOVE "49"           TO   SGY-F04
     END-IF.
     IF  YNY-F15  =  "1T1"
         MOVE "TI"           TO   SGY-F04
     END-IF.
*作業日
     MOVE     WK-DATE8      TO   SGY-F05.
*入出庫区分
     MOVE     "1"           TO   SGY-F06.
*ストックＮＯ
     MOVE     SPACE         TO   SGY-F07.
*商品ＣＤ
     MOVE     MEI-F06       TO   SGY-F08.
*品単ＣＤ
     MOVE     MEI-F07       TO   SGY-F09.
*_番
     MOVE     MEI-F08       TO   SGY-F10.
*数量　　　
     MOVE     MEI-F09       TO   SGY-F11.
*データ種別
     MOVE     DND-F04       TO   SGY-F89.
*登録日
     MOVE     WK-DATE8      TO   SGY-F98.
*修正日
     MOVE     WK-DATE8      TO   SGY-F99.
     WRITE    SGY-REC.
     ADD      1             TO   WK-SG-CNT.
*
 SGYFILF-WT-EXIT.
     EXIT.
****************************************************************
*               入荷予定累積ファイル作成
****************************************************************
 NYKRUIF-WT-SEC              SECTION.
     MOVE     "NYKRUIF-WT-SEC"    TO   S-NAME.
*存在チェック
     MOVE     DND-F11        TO   RUI-F01
     MOVE     DND-F09        TO   RUI-F02.
*****MOVE     DND-F10        TO   RUI-F03.
*#2021/06/08 NAV ST
     MOVE     1              TO   RUI-F03.
*#2021/06/08 NAV ED
     READ  NYKRUIF
           INVALID     MOVE  "INV"     TO   NYKRUIF-INV-FLG
           NOT INVALID MOVE  SPACE     TO   NYKRUIF-INV-FLG
     END-READ.
*
     IF    NYKRUIF-INV-FLG  =  SPACE
           DISPLAY NC"＃同一キーが存在します！＃" UPON CONS
           DISPLAY NC"＃Ｄ伝票_" " = " RUI-F01   UPON CONS
           DISPLAY NC"＃伝票番号" " = " RUI-F02   UPON CONS
           DISPLAY NC"＃行番号　" " = " RUI-F03   UPON CONS
           GO                TO   NYKRUIF-WT-EXIT
     END-IF.
*
     MOVE     SPACE          TO   RUI-REC.
     INITIALIZE                   RUI-REC.
*キーセット
     MOVE     DND-F11        TO   RUI-F01
     MOVE     DND-F09        TO   RUI-F02.
*#2021/06/08 NAV ST
*****MOVE     DND-F10        TO   RUI-F03.
     MOVE     1              TO   RUI-F03.
*#2021/06/08 NAV ED
*データ種別
     MOVE     DND-F04        TO   RUI-F04.
*入荷予定レコードセット
     MOVE     YNY-REC        TO   RUI-F06.
*レコード作成
     WRITE  RUI-REC.
     ADD      1              TO   WK-RU-CNT.
*
 NYKRUIF-WT-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    YNY-F15(2:2)    TO   ZAI-F01.
*****特別処理
     IF  YNY-F15  =  "106"
         MOVE "01"           TO   ZAI-F01
     END-IF.
     IF  YNY-F15  =  "107"
         MOVE "41"           TO   ZAI-F01
     END-IF.
     IF  YNY-F15  =  "149"
         MOVE "49"           TO   ZAI-F01
     END-IF.
     IF  YNY-F15  =  "1T1"
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
      MOVE      YNY-F15(2:2)  TO   ZAI-F01.
*****特別処理
     IF  YNY-F15  =  "106"
         MOVE "01"           TO   ZAI-F01
     END-IF.
     IF  YNY-F15  =  "107"
         MOVE "41"           TO   ZAI-F01
     END-IF.
     IF  YNY-F15  =  "149"
         MOVE "49"           TO   ZAI-F01
     END-IF.
     IF  YNY-F15  =  "1T1"
         MOVE "TI"           TO   ZAI-F01
     END-IF.
      MOVE      SBM-F011      TO   ZAI-F021.
      MOVE      SBM-F012      TO   ZAI-F022.
      MOVE      MEI-F08       TO   ZAI-F03.
*未入庫数加算
*#2020/11/26 NAV ST
******COMPUTE   ZAI-F26       =    ZAI-F26  +  YNY-F11.
      COMPUTE   ZAI-F26       =    ZAI-F26  +  WK-YNY-F11.
*#2020/11/26 NAV ED
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
*#2020/11/26 NAV ST
*****COMPUTE   ZAI-F26       =    ZAI-F26  +  YNY-F11.
     COMPUTE   ZAI-F26       =    ZAI-F26  +  WK-YNY-F11.
*#2020/11/26 NAV ED
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
*              倉庫別商品別_番設定マスタ読込　　              *
****************************************************************
 SOKTANF-READ-SEC          SECTION.
*
     MOVE      "SOKTANF-READ-SEC" TO    S-NAME.
*
     READ      SOKTANF
               INVALID
               MOVE      "INV"    TO    SOKTANF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SOKTANF-INV-FLG
     END-READ.
*
 SOKTANF-READ-EXIT.
     EXIT.
****************************************************************
*              条件ファイル読込　　　　　　　　　              *
****************************************************************
 HJYOKEN-READ-SEC          SECTION.
*
     MOVE      "HJYOKEN-READ-SEC" TO    S-NAME.
*
     READ      HJYOKEN
               INVALID
               MOVE      "INV"    TO    HJYOKEN-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*              作業番号採番　　　　　　　　　　　              *
****************************************************************
 SGYNO-SAIBAN-SEC          SECTION.
*
     MOVE      "SGYNO-SAIBAN-SEC" TO    S-NAME.
*
     OPEN  I-O  HJYOKEN.
*
     MOVE      62                 TO    JYO-F01.
     MOVE      SPACE              TO    JYO-F02.
     READ      HJYOKEN
               INVALID
               MOVE      "INV"    TO    HJYOKEN-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HJYOKEN-INV-FLG
     END-READ.
*
     IF  HJYOKEN-INV-FLG  =  "INV"
         DISPLAY NC"＃条件Ｆ作業ＮＯ　採番エラー！＃" UPON CONS
         MOVE  4000               TO    PROGRAM-STATUS
         STOP  RUN
     END-IF.
*
     MOVE      JYO-F04            TO    WK-JYO-F04.
*
     ADD       1                  TO    JYO-F04.
     IF  JYO-F04  >  9999999
         MOVE  1                  TO    JYO-F04
     END-IF.
*
     REWRITE  JYO-REC.
*
     CLOSE  HJYOKEN.
*
 SGYNO-SAIBAN-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     DISPLAY "READ-CNT  = "  WK-RD-CNT  UPON CONS.
     DISPLAY "HED-CNT   = "  WK-HD-CNT  UPON CONS.
     DISPLAY "MEI-CNT   = "  WK-ME-CNT  UPON CONS.
     DISPLAY "SGY-CNT   = "  WK-SG-CNT  UPON CONS.
     DISPLAY "RUI-CNT   = "  WK-RU-CNT  UPON CONS.
*
     CLOSE       HACHEDF  HACMEIF  ZAMZAIF  DNDTKNF
                 SGYFILF  NYKRUIF
                 SUBMEIF  SOKTANF.
 END-EXIT.
     EXIT.
