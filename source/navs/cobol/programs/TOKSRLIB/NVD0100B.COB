****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定データ取込＆チェック　　　*
*    作成日／作成者　　　：　2019/12/25   ASS.TAKAHASHI        *
*    処理内容　　　　　　：　入荷予定取込データＦを読み、　　　*
*    　　　　　　　　　　　　各マスタチェックを行う。　　　　　*
*                            自社伝票番号の採番を行う。　　　　*
*    更新日／更新者　　　：　2020/10/05   NAV TAKAHASHI        *
*    更新内容　　　　　　：　仕入先ＣＤ未登録時対応　　　　　　*
*　　　　　　　　　　　　　　ＪＡＮＣＤ＝空白時対応　　　　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVD0100B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*入荷予定データ
     SELECT  YTINYKF  ASSIGN        TO  DA-01-YTINYKF
                      ORGANIZATION  IS  SEQUENTIAL
                      ACCESS MODE   IS  SEQUENTIAL
                      FILE   STATUS IS  YTI-ST.
*条件ファイル(READ)
     SELECT  JYOKENRD ASSIGN        TO  JYOKEN1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  JRD-F01
                                        JRD-F02
                      FILE   STATUS IS  JRD-ST.
*条件ファイル(UPDATE)
     SELECT  JYOKENUP ASSIGN        TO  HJYOKEN
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  JUP-F01
                                        JUP-F02
                      FILE   STATUS IS  JUP-ST.
*サブ商品名称マスタ
     SELECT  SUBMEIF  ASSIGN        TO  SUBMEIL7
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  SUB-D01
*                             WITH DUPLICATES
                      FILE   STATUS IS  SUB-ST.
*仕入先マスタ
     SELECT  ZSHIMS   ASSIGN        TO  ZSHIMS1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  SHI-F01
                      FILE   STATUS IS  SHI-ST.
*倉庫マスタ
     SELECT  ZSOKMS   ASSIGN        TO  ZSOKMS1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  SOK-F01
                      FILE   STATUS IS  SOK-ST.
*ＮＡＶＳ受信データ管理ファイル
     SELECT  DNDTKNF  ASSIGN        TO  DNDTKNL1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  DND-F01 *> 送受信指示NO
                                        DND-F07 *> 取込日付
                                        DND-F08 *> 取込時刻
                                        DND-F09 *> 伝票番号
                                        DND-F10 *> 行番号
                      FILE   STATUS IS  DND-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*入荷予定データ
 FD  YTINYKF
     LABEL       RECORD      IS        STANDARD.
*### BLOCK       CONTAINS    31        RECORDS.
*###   COPY   YTINYKF
*###          DISJOINING  XXX  JOINING  YTI  AS PREFIX.
      COPY        YTINYKF     OF        XFDLIB
      JOINING     YTI         AS        PREFIX.
*条件ファイル(READ)
 FD  JYOKENRD.
*###   COPY   HJYOKEN
*###          DISJOINING  XXX  JOINING  JRD  AS PREFIX.
     COPY        HJYOKEN     OF        XFDLIB
     JOINING     JRD         AS        PREFIX.
*条件ファイル(UPDATE)
 FD  JYOKENUP.
*###   COPY   HJYOKEN
*###          DISJOINING  XXX  JOINING  JUP  AS PREFIX.
     COPY        HJYOKEN     OF        XFDLIB
     JOINING     JUP         AS        PREFIX.
*サブ商品名称マスタ
 FD  SUBMEIF.
*###   COPY   SUBMEIF
*###          DISJOINING  XXX  JOINING  SUB  AS PREFIX.
     COPY        SUBMEIF     OF        XFDLIB
     JOINING     SUB         AS        PREFIX.
*仕入先マスタ
 FD  ZSHIMS.
*###   COPY   ZSHIMS
*###          DISJOINING  XXX  JOINING  SHI  AS PREFIX.
     COPY        ZSHIMS      OF        XFDLIB
     JOINING     SHI         AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS.
*###   COPY   ZSOKMS
*###          DISJOINING  XXX  JOINING  SOK  AS PREFIX.
     COPY        ZSOKMS      OF        XFDLIB
     JOINING     SOK         AS        PREFIX.
*ＮＡＶＳ受信データ管理ファイル
 FD  DNDTKNF.
*###   COPY   DNDTKNF
*###          DISJOINING  XXX  JOINING  DND  AS PREFIX.
     COPY        DNDTKNF     OF        XFDLIB
     JOINING     DND         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  YTI-ST              PIC  X(02)  VALUE  SPACE.
     03  JRD-ST              PIC  X(02)  VALUE  SPACE.
     03  JUP-ST              PIC  X(02)  VALUE  SPACE.
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
     03  SHI-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
     03  DND-ST              PIC  X(02)  VALUE  SPACE.

 01  END-FLG                 PIC  X(03)  VALUE  SPACE.

 01  CNT-AREA.
     03  YTI-CNT             PIC  9(07)  VALUE  ZERO.
     03  DND-CNT             PIC  9(07)  VALUE  ZERO.
     03  OK-CNT              PIC  9(07)  VALUE  ZERO.
     03  NG-CNT              PIC  9(07)  VALUE  ZERO.
 01  FLG-AREA.
     03  SUBMEIF-INV-FLG     PIC  X(03).
     03  ZSOKMS-INV-FLG      PIC  X(03).
     03  ZSHIMS-INV-FLG      PIC  X(03).
     03  JYOKEN-INV-FLG      PIC  X(03).
     03  DNDTKNF-INV-FLG     PIC  X(03).

 01  WK-AREA.
     03  WK-DENNO            PIC  X(20).
     03  WK-NAVS-DENNO       PIC  X(10).
     03  WK-NAVS-DENNO-R          REDEFINES WK-NAVS-DENNO.
         05  WK-NAVS         PIC  9(10).
     03  WK-NAVS-MIN         PIC  9(10).
     03  WK-NAVS-MAX         PIC  9(10).

 01  CHK-AREA.
     03  CHK-ERR-KBN         PIC  X(01).
     03  CHK-ERR             PIC  X(01)  OCCURS 10.

*日付取得
 01  SYS-DATE                PIC  9(06).
 01  WK-DATE8.
     03  WK-Y                PIC  9(04).
     03  WK-M                PIC  9(02).
     03  WK-D                PIC  9(02).
 01  SYS-TIME.
     03  WK-TIME             PIC  9(06).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  YTI-ERR-NM         PIC  N(10)  VALUE
                   NC"入荷予定データ　異常".
     03  JRD-ERR-NM          PIC  N(10)  VALUE
                   NC"条件Ｆ（読込用）異常".
     03  JUP-ERR-NM          PIC  N(10)  VALUE
                   NC"条件Ｆ（更新用）異常".
     03  SUB-ERR-NM          PIC  N(10)  VALUE
                   NC"サブ商品名称Ｍ　異常".
     03  SHI-ERR-NM          PIC  N(10)  VALUE
                   NC"仕入先マスタ　異常".
     03  SOK-ERR-NM          PIC  N(10)  VALUE
                   NC"倉庫マスタ　異常".
     03  DND-ERR-NM          PIC  N(10)  VALUE
                   NC"受信データ管理Ｆ異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NVD0100B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NVD0100B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 入荷予定データ　読込件数 = ".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 受信Ｄ管理Ｆ　　作成件数 = ".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  PARA-SOJNO              PIC  9(10).
 01  PARA-SOJKBN             PIC  X(01).
 01  PARA-KUBUN              PIC  X(01).
 01  PARA-SYUBETU            PIC  X(02).
 01  PARA-SIJIDATE           PIC  9(08).
 01  PARA-SIJITIME           PIC  9(06).
 01  PARA-CNT-ALL            PIC  9(07).
 01  PARA-CNT-OK             PIC  9(07).
 01  PARA-CNT-NG             PIC  9(07).
****************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-SOJNO
                                      PARA-SOJKBN
                                      PARA-KUBUN
                                      PARA-SYUBETU
                                      PARA-SIJIDATE
                                      PARA-SIJITIME
                                      PARA-CNT-ALL
                                      PARA-CNT-OK
                                      PARA-CNT-NG.
****************************************************************
 DECLARATIVES.
 YTI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE YTINYKF.
     DISPLAY     YTI-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     YTI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JRD-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKENRD.
     DISPLAY     JRD-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JRD-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JUP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKENUP.
     DISPLAY     JUP-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JUP-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SUB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIF.
     DISPLAY     SUB-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SUB-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SHI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZSHIMS.
     DISPLAY     SHI-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 DND-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DNDTKNF.
     DISPLAY     DND-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DND-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*    初期処理
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.

*#### TEST <<<<<<
*### DISPLAY " 送受信指示Ｎｏ --- "  PARA-SOJNO    UPON CONS.
*### DISPLAY " 送受信区分 ------- "  PARA-SOJKBN   UPON CONS.
*### DISPLAY " 自動手動区分 ----- "  PARA-KUBUN    UPON CONS.
*### DISPLAY " データ種別 ------- "  PARA-SYUBETU  UPON CONS.
*### DISPLAY " 指示日付 --------- "  PARA-SIJIDATE UPON CONS.
*### DISPLAY " 指示時刻 --------- "  PARA-SIJITIME UPON CONS.
*### DISPLAY "  " UPON CONS.
*#### TEST >>>>>>
*
     DISPLAY     MSG-START    UPON  CONS.
*
     OPEN        INPUT       YTINYKF
                             JYOKENRD
                             SUBMEIF
                             ZSHIMS
                             ZSOKMS.
     OPEN        I-O         DNDTKNF.

*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
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

     INITIALIZE             WK-AREA.
*
*入荷予定データを読み込む
     PERFORM  YTINYKF-READ-SEC.
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4000     TO    PROGRAM-STATUS
          STOP     RUN
     END-IF.

*伝票番号採番用に条件ファイルを検索
     MOVE     60                  TO   JRD-F01.
     MOVE     SPACE               TO   JRD-F02.
     PERFORM  JYOKEN-READ-SEC.
     IF   JYOKEN-INV-FLG  =  SPACE
          MOVE     JRD-F04        TO   WK-NAVS
          MOVE     1              TO   WK-NAVS-MIN
          MOVE     9999999        TO   WK-NAVS-MAX
     ELSE
          DISPLAY "＃＃条件ファイルなし(KEY=60, )＃＃" UPON
          CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4000     TO    PROGRAM-STATUS
          STOP     RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    主処理
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.

*ＮＡＶＳ採番伝票番号を採番する
*2021/06/08↓
*    IF  WK-DENNO    NOT =  YTI-F01
*        IF  WK-NAVS  <  WK-NAVS-MAX
*            ADD   1              TO  WK-NAVS
*        ELSE
*            MOVE   WK-NAVS-MIN   TO  WK-NAVS
*        END-IF
*        MOVE  YTI-F01            TO  WK-DENNO
*    END-IF.
     IF  WK-NAVS  <  WK-NAVS-MAX
         ADD   1              TO  WK-NAVS
     ELSE
         MOVE   WK-NAVS-MIN   TO  WK-NAVS
     END-IF.
*2021/06/08↑
*
*チェック処理
     PERFORM  CHECK-SEC.
     IF  DNDTKNF-INV-FLG  =  SPACE
         GO    TO        MAIN-900
     END-IF.

*チェック結果のカウント
     IF  CHK-ERR-KBN  =  SPACE
         ADD   1         TO   OK-CNT
     ELSE
         ADD   1         TO   NG-CNT
     END-IF.

*編集処理
     PERFORM  HENSYU-SEC.
*出力処理
     WRITE    DND-REC.
     ADD      1          TO   DND-CNT.

 MAIN-900.
*次の入荷予定データを読み込む
     PERFORM  YTINYKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    入荷予定データ　順読込
****************************************************************
 YTINYKF-READ-SEC            SECTION.
     MOVE     "YTINYKF-READ-SEC"  TO   S-NAME.
*
     READ     YTINYKF
         AT END
              MOVE   "END"        TO   END-FLG
              GO      TO          YTINYKF-READ-EXIT
     END-READ.
*
     ADD      1   TO    YTI-CNT.
*
 YTINYKF-READ-EXIT.
     EXIT.
***************************************************************
*    チェック処理
***************************************************************
 CHECK-SEC                  SECTION.
     MOVE     "CHECK-SEC"        TO   S-NAME.
*
     MOVE     SPACE              TO   CHK-AREA.

*ＮＡＶＳ受信データ管理ファイルの重複チェック
     MOVE    PARA-SOJNO          TO  DND-F01.
     MOVE    WK-DATE8            TO  DND-F07.
     MOVE    WK-TIME             TO  DND-F08.
     MOVE    YTI-F01             TO  DND-F09.
     MOVE    YTI-F02             TO  DND-F10.
     PERFORM  DNDTKNF-READ-SEC.
     IF  DNDTKNF-INV-FLG  =  SPACE
         MOVE   "1"              TO  CHK-ERR-KBN
         MOVE   "1"              TO  CHK-ERR(1)
     END-IF.

*商品マスタ（ＪＡＮＣＤ）存在チェック
     MOVE     YTI-F08            TO  SUB-D01.
     PERFORM  SUBMEIF-READ-SEC.
     IF  SUBMEIF-INV-FLG  NOT =  SPACE
         MOVE   "1"              TO  CHK-ERR-KBN
         MOVE   "1"              TO  CHK-ERR(2)
     END-IF.
*#2020/10/05 NAV ST Ｄ３６５のＪＡＮが空白の場合エラー
     IF       YTI-F08   =  SPACE
         MOVE   "1"              TO  CHK-ERR-KBN
         MOVE   "1"              TO  CHK-ERR(6)
     END-IF.
*#2020/10/05 NAV ED Ｄ３６５のＪＡＮが空白の場合エラー

*倉庫マスタ（場所）存在チェック
     MOVE     YTI-F15(2:2)       TO  SOK-F01.
     PERFORM  ZSOKMS-READ-SEC.
*****特別処理
     IF  YTI-F15  =  "106"
         MOVE SPACE              TO  ZSOKMS-INV-FLG
     END-IF.
     IF  YTI-F15  =  "107"
         MOVE SPACE              TO  ZSOKMS-INV-FLG
     END-IF.
     IF  YTI-F15  =  "149"
         MOVE SPACE              TO  ZSOKMS-INV-FLG
     END-IF.
     IF  YTI-F15  =  "1T1"
         MOVE SPACE              TO  ZSOKMS-INV-FLG
     END-IF.
     IF  ZSOKMS-INV-FLG   NOT =  SPACE
         MOVE   "1"              TO  CHK-ERR-KBN
         MOVE   "1"              TO  CHK-ERR(3)
     END-IF.

*仕入先マスタ（仕入先）存在チェック
     IF  YTI-F03  NOT =  SPACE
         MOVE     YTI-F03            TO  SHI-F01
         PERFORM  ZSHIMS-READ-SEC
         IF  ZSHIMS-INV-FLG   NOT =  SPACE
*********#2020/10/05 NAV ST 仕入先マスタ未登録の場合固定値セット
*************MOVE   "1"              TO  CHK-ERR-KBN
*************MOVE   "1"              TO  CHK-ERR(4)
             MOVE   "D365SIIR            " TO YTI-F03
*********#2020/10/05 NAV ED
         END-IF
     ELSE     *>未セットの場合、下記をセット
         MOVE  "D365SIIR            " TO YTI-F03
     END-IF.

*日付（発注日・納品）論理チェック
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     ZERO                TO   LINK-IN-YMD6.
     MOVE     YTI-F04(1:4)        TO   LINK-IN-YMD8(1:4).
     MOVE     YTI-F04(6:2)        TO   LINK-IN-YMD8(5:2).
     MOVE     YTI-F04(9:2)        TO   LINK-IN-YMD8(7:2).
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF  LINK-OUT-RET    NOT =  ZERO
         MOVE   "1"              TO  CHK-ERR-KBN
         MOVE   "1"              TO  CHK-ERR(5)
     END-IF.
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     ZERO                TO   LINK-IN-YMD6.
     MOVE     YTI-F05(1:4)        TO   LINK-IN-YMD8(1:4).
     MOVE     YTI-F05(6:2)        TO   LINK-IN-YMD8(5:2).
     MOVE     YTI-F05(9:2)        TO   LINK-IN-YMD8(7:2).
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF  LINK-OUT-RET    NOT =  ZERO
         MOVE   "1"              TO  CHK-ERR-KBN
         MOVE   "1"              TO  CHK-ERR(5)
     END-IF.
*
 CHECK-EXIT.
     EXIT.
***************************************************************
*    編集処理
***************************************************************
 HENSYU-SEC                 SECTION.
     MOVE     "HENSYU-SEC"       TO   S-NAME.
*
     MOVE     SPACE              TO  DND-REC.
     INITIALIZE                      DND-REC.

*送受信指示Ｎｏ
     MOVE      PARA-SOJNO        TO  DND-F01.
*送受信区分
     MOVE      PARA-SOJKBN       TO  DND-F02.
*自動手動区分
     MOVE      PARA-KUBUN        TO  DND-F03.
*送受信データ種別区分
     MOVE      PARA-SYUBETU      TO  DND-F04.
*指示日付
     MOVE      PARA-SIJIDATE     TO  DND-F05.
*指示時刻
     MOVE      PARA-SIJITIME     TO  DND-F06.
*取込日付
     MOVE      WK-DATE8          TO  DND-F07.
*取込時刻
     MOVE      WK-TIME           TO  DND-F08.
*伝票番号
     MOVE      YTI-F01           TO  DND-F09.
*行番号
     MOVE      YTI-F02           TO  DND-F10.
*ＮＡＶＳ採番伝票番号
     MOVE      WK-NAVS-DENNO     TO  DND-F11.
*入庫予定データ
     MOVE      YTI-REC           TO  DND-F20.
*エラー情報
     MOVE      CHK-AREA          TO  DND-F21.
*
 HENSYU-EXIT.
     EXIT.
****************************************************************
*    サブ商品名称マスタ・読込み
****************************************************************
 SUBMEIF-READ-SEC           SECTION.
     MOVE     "SUBMEIF-READ-SEC" TO    S-NAME.
*
     MOVE     SPACE              TO    SUBMEIF-INV-FLG.

     READ      SUBMEIF
          INVALID
               MOVE   "INV"      TO    SUBMEIF-INV-FLG
     END-READ.
*
 SUBMEIF-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ・読込み
****************************************************************
 ZSOKMS-READ-SEC            SECTION.
     MOVE     "ZSOKMS-READ-SEC"  TO    S-NAME.
*
     MOVE     SPACE              TO    ZSOKMS-INV-FLG.

     READ      ZSOKMS
          INVALID
               MOVE   "INV"      TO    ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    仕入先マスタ・読込み
****************************************************************
 ZSHIMS-READ-SEC            SECTION.
     MOVE     "ZSHIMS-READ-SEC"  TO    S-NAME.
*
     MOVE     SPACE              TO    ZSHIMS-INV-FLG.

     READ      ZSHIMS
          INVALID
               MOVE   "INV"      TO    ZSHIMS-INV-FLG
     END-READ.
*
 ZSHIMS-READ-EXIT.
     EXIT.
****************************************************************
*    ＮＡＶＳ受信データ管理ファイル・読込み
****************************************************************
 DNDTKNF-READ-SEC           SECTION.
     MOVE     "DNDTKNF-READ-SEC" TO    S-NAME.
*
     MOVE     SPACE              TO    DNDTKNF-INV-FLG.

     READ      DNDTKNF
          INVALID
               MOVE   "INV"      TO    DNDTKNF-INV-FLG
     END-READ.
*
 DNDTKNF-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル・読込み
****************************************************************
 JYOKEN-READ-SEC            SECTION.
     MOVE     "JYOKEN-READ-SEC"  TO    S-NAME.
*
     MOVE     SPACE              TO    JYOKEN-INV-FLG.

     READ     JYOKENRD
          INVALID
               MOVE   "INV"      TO    JYOKEN-INV-FLG
     END-READ.
*
 JYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル・更新
****************************************************************
 JYOKEN-UPDATE-SEC          SECTION.
     MOVE     "JYOKEN-UPDATE-SEC" TO    S-NAME.
*
     OPEN  I-O  JYOKENUP.

     MOVE     60                  TO   JUP-F01.
     MOVE     SPACE               TO   JUP-F02.
     READ     JYOKENUP
          INVALID
          DISPLAY "＃＃条件ファイル更新エラー　＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4000     TO    PROGRAM-STATUS
          STOP     RUN
     END-READ.

     MOVE     WK-NAVS             TO   JUP-F04.
     REWRITE  JUP-REC.

     CLOSE      JYOKENUP.
*
 JYOKEN-UPDATE-EXIT.
     EXIT.
****************************************************************
*    終了処理
****************************************************************
 END-SEC                   SECTION.
     MOVE     "END-SEC"         TO   S-NAME.

*最終の伝票番号を条件ファイルに反映
     PERFORM  JYOKEN-UPDATE-SEC.
*
     MOVE     YTI-CNT           TO   MSG-OUT01.
     MOVE     DND-CNT           TO   MSG-OUT02.
     DISPLAY  MSG-OUT1        UPON   CONS.
     DISPLAY  MSG-OUT2        UPON   CONS.
*
     CLOSE    YTINYKF
              JYOKENRD
              SUBMEIF
              ZSHIMS
              ZSOKMS
              DNDTKNF.

*ＯＵＴパラメタセット
     MOVE     YTI-CNT           TO   PARA-CNT-ALL.
     MOVE     OK-CNT            TO   PARA-CNT-OK.
     MOVE     NG-CNT            TO   PARA-CNT-NG.

     DISPLAY  MSG-END         UPON   CONS.

*#### TEST <<<<<<
*####DISPLAY " " UPON CONS.
*####DISPLAY " 件数（全件） ----- "  PARA-CNT-ALL UPON CONS.
*####DISPLAY " 件数（内ＯＫ）---- "  PARA-CNT-OK  UPON CONS.
*####DISPLAY " 件数（内ＮＧ）---- "  PARA-CNT-NG  UPON CONS.
     DISPLAY " 件数（全件） ----- "  YTI-CNT      UPON CONS.
     DISPLAY " 件数（内ＯＫ）---- "  OK-CNT       UPON CONS.
     DISPLAY " 件数（内ＮＧ）---- "  NG-CNT       UPON CONS.
*####DISPLAY "  " UPON CONS.
*#### TEST >>>>>>

 END-EXIT.
     EXIT.
