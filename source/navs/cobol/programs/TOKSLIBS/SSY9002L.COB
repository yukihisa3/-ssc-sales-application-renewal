****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　オージョイフルオンラインシステム  *
*    モジュール名　　　　：　オージョイフル仕入伝票発行　　    *
*    作成日／更新日　　　：　08/05/29                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　ＴＡ_型                          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY9001L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/05/29.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA             IS        YA
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票データ（共通）　　
     SELECT   JHSHENL1  ASSIGN    TO        JHSHENL1
                        ACCESS    MODE IS   SEQUENTIAL
                        STATUS              DEN-ST.
*画面定義ファイル　　
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING          DSP-PRO
                        FUNCTION            DSP-FNC
                        STATUS              DSP-ST.
*プリント定義ファイル　　
     SELECT   PRTFILE   ASSIGN    TO        LP-04-PRTF
                        STATUS              PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
****************************************************************
*  FILE =伝票データ（共通）　                                  *
****************************************************************
 FD  JHSHENL1
                        BLOCK    CONTAINS  1    RECORDS
                        LABEL    RECORD    IS   STANDARD.
                        COPY     OJSHIRED  OF   XFDLIB
                        JOINING  DEN       AS   PREFIX.
****************************************************************
*    FILE = ｶﾞﾒﾝ  F                                            *
****************************************************************
 FD  DSPFILE
                        LABEL RECORD   IS   OMITTED.
*
     COPY    FSY11011   OF   XMDLIB.
*
****************************************************************
*    FILE = ﾌﾟﾘﾝﾄ ﾌｱｲﾙ                                         *
****************************************************************
 FD  PRTFILE
                        LABEL RECORD   IS   OMITTED
                        LINAGE IS      30   LINES
                        DATA  RECORD   IS   PRT-REC.
 01  PRT-REC.
     03  FILLER                   PIC  X(200).
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
***  ｽﾃｰﾀｽｴﾘｱ
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
***  ｶﾞﾒﾝ ｺﾝﾄﾛｰﾙ ｴﾘｱ
 01  DSP-CNTL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
     03  DSP-ST                   PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE  ZERO.
     03  PROGRAM-END              PIC  X(03)     VALUE  ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE  ZERO.
     03  KAI-FLG                  PIC  9(01)     VALUE  ZERO.
     03  SYU-FLG                  PIC  9(01)     VALUE  ZERO.
     03  ZEN-FLG                  PIC  9(01)     VALUE  ZERO.
     03  TS-FLG                   PIC  9(01)     VALUE  ZERO.
***  ｶｳﾝﾄ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(07)     VALUE  ZERO.
     03  CNT-AFTER                PIC  9(07)     VALUE  ZERO.
     03  READ-CNT                 PIC  9(08)     VALUE  ZERO.
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
***  ｺﾞｳｹｲ
 01  GOUKEI.
     03  GENKINKEI                PIC  9(09)     VALUE  ZERO.
     03  BAIKINKEI                PIC  9(09)     VALUE  ZERO.
     03  SURYOKEI                 PIC  9(06)V9   VALUE  ZERO.
     03  TSGENKEI                 PIC  9(09)     VALUE  ZERO.
     03  TSBAIKEI                 PIC  9(09)     VALUE  ZERO.
     03  TSSURYOKEI               PIC  9(06)V9   VALUE  ZERO.
***  ﾜｰｸ ｴﾘｱ
 01  WRK-AREA.
     03  WK-MAI                   PIC  9(06)     VALUE  ZERO.
     03  RD-SW                    PIC  9(01)     VALUE  ZERO.
     03  IN-DATA                  PIC  X(01).
     03  DEN-NO                   PIC  9(09)     VALUE  ZERO.
     03  CNT-DENPYO               PIC  9(07)     VALUE  ZERO.
     03  ZERO-FLG                 PIC  9(01)     VALUE  ZERO.
     03  I                        PIC  9(02)     VALUE  ZERO.
     03  J                        PIC  9(02)     VALUE  ZERO.
     03  WK-DEN-NO                PIC  9(09)     VALUE  ZERO.
     03  WK-DEN-A05               PIC  9(05)     VALUE  ZERO.
     03  WK-DEN-A06               PIC  9(04)     VALUE  ZERO.
     03  WK-DEN-A07               PIC  9(02)     VALUE  ZERO.
     03  WK-DEN-A03               PIC  9(09)     VALUE  ZERO.
     03  WK-DEN-A10               PIC  9(06)     VALUE  ZERO.
     03  WK-DEN-A08               PIC  9(06)     VALUE  ZERO.
     03  WK-DEN-A09               PIC  9(06)     VALUE  ZERO.
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
***  ﾃﾞﾝﾋﾟﾖｳ ｷ-
 01  WRK-DENNO.
     03  DENNO1                   PIC  9(09).
***  ﾒｯｾｰｼﾞｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"　　　　　　　　　　　　　　　".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"仕入伝票発行中".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     4      PIC  N(30).
*
 01  FILE-ERR010                  PIC  N(15)     VALUE
         NC"プリンター　異常！！".
 01  FILE-ERR020                  PIC  N(15)     VALUE
         NC"仕入ファイル　異常！！".
*01  FILE-ERR030                  PIC  N(15)     VALUE
*        NC"画面ファイル　異常！！".
***  ﾍﾝｼｭｳ ｴﾘｱ
***  数量
 01  WK-SURYO                     PIC  9(06)V9.
 01  WK-SURYO-R  REDEFINES  WK-SURYO.
     03  WK-SURYO1                PIC  9(06).
     03  WK-SURYO2                PIC  9(01).
***  原価単価
 01  WK-GENKA                     PIC  9(06)V99.
 01  WK-GENKA-R  REDEFINES  WK-GENKA.
     03  WK-GENKA1                PIC  9(06).
     03  WK-GENKA2                PIC  9(02).
***  画面用日付取得
*01  WK-DATE8.
*    03  WK-DATE8-YY1             PIC  9(02).
*    03  WK-DATE8-YY2             PIC  9(06).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1                          CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(80)     VALUE   SPACE.
     03  HD1-01                   PIC  N(01).
     03  FILLER                   PIC  X(12)     VALUE   SPACE.
     03  HD1-02                   PIC  X(17).
*
 01  HD2.
     03  FILLER                   PIC  X(08)     VALUE   SPACE.
     03  HD2-01                   PIC  X(20).
*
 01  HD3.
     03  FILLER                   PIC  X(08)     VALUE   SPACE.
     03  HD3-01                   PIC  X(20).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  HD3-02                   PIC  9(06).
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  HD3-03                   PIC  X(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD3-04                   PIC  X(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD3-05                   PIC  ZZ9999999.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  HD3-06                   PIC  9(05).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  HD3-07                   PIC  X(20).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-081                  PIC  99.
     03  HD3-082                  PIC  Z9.
     03  HD3-083                  PIC  Z9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-091                  PIC  99.
     03  HD3-092                  PIC  Z9.
     03  HD3-093                  PIC  Z9.
*
*--------------------------------------------------------------*
*    明細                                                      *
*--------------------------------------------------------------*
*
 01  DT1.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  DT1-01                   PIC  X(25).
     03  FILLER                   PIC  X(55)     VALUE   SPACE.
     03  DT1-02                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06).
     03  DT1-03                   PIC  ZZZZZZZZ9.
*
 01  DT2.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  DT2-01                   PIC  X(06).
     03  FILLER                   PIC  X(19)     VALUE   SPACE.
     03  DT2-02                   PIC  X(13).
     03  FILLER                   PIC  X(15)     VALUE   SPACE.
     03  DT2-031                  PIC  ZZZZ9.
     03  DT2-032                  PIC  Z.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  DT2-041                  PIC  ZZZZZ9.
     03  DT2-042                  PIC  Z.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  DT2-051                  PIC  ZZZZZ9.
     03  DT2-052                  PIC  99.
     03  DT2-06                   PIC  ZZZZZZZZ9.
     03  DT2-07                   PIC  ZZZZZ9.
     03  DT2-08                   PIC  ZZZZZZZZ9.
*伝票訂正用（ＤＴ・ＴＬ共通）
 01  DT21.
     03  FILLER                   PIC  X(59)     VALUE SPACE.
     03  FILLER                   PIC  X(06)      VALUE
                                                 "======".
     03  FILLER                   PIC  X(21)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
     03  FILLER                   PIC  X(06)     VALUE SPACE.
     03  FILLER                   PIC  X(09)     VALUE
                                                 "=========".
*--------------------------------------------------------------*
*    テール行                                                  *
*--------------------------------------------------------------*
 01  TL1.
     03  FILLER                   PIC  X(59)     VALUE   SPACE.
     03  TL1-011                  PIC  ZZZZ9.
     03  TL1-012                  PIC  Z.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  TL1-021                  PIC  ZZZZZ9.
     03  TL1-022                  PIC  Z.
     03  FILLER                   PIC  X(12)     VALUE   SPACE.
     03  TL1-03                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL1-04                   PIC  ZZZZZZZZ9.
*伝票訂正用（合計欄）
 01  TL2.
     03  FILLER                   PIC  X(73)     VALUE SPACE.
     03  TL2-01                   PIC  X(18).
     03  FILLER                   PIC  X(01)     VALUE SPACE.
     03  TL2-02                   PIC  X(18).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE     SECTION.
 01  LINK-JDATE             PIC  9(08).
 01  LINK-JTIME             PIC  9(04).
 01  LINK-TORICD            PIC  9(08).
 01  LINK-SOKO              PIC  X(02).
 01  LINK-STENCD            PIC  9(05).
 01  LINK-ETENCD            PIC  9(05).
 01  LINK-SNOUDT            PIC  9(08).
 01  LINK-ENOUDT            PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-JDATE
                                         LINK-JTIME
                                         LINK-TORICD
                                         LINK-SOKO
                                         LINK-STENCD
                                         LINK-ETENCD
                                         LINK-SNOUDT
                                         LINK-ENOUDT.
****************************************************************
 DECLARATIVES.
*--- << プリンタエラー >> ---*
 000-PRTFILE-ERR        SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTFILE.
     DISPLAY  FILE-ERR010         UPON    CONS.
     DISPLAY  SEC-NAME            UPON    CONS.
     DISPLAY  PRT-ST              UPON    CONS.
     ACCEPT   IN-DATA             FROM    CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
*--- << 仕入データエラー >> ---*
 000-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      JHSHENL1.
     DISPLAY  FILE-ERR020         UPON    CONS.
     DISPLAY  SEC-NAME            UPON    CONS.
     DISPLAY  DEN-ST              UPON    CONS.
     ACCEPT   IN-DATA             FROM    CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
*--- << 画面エラー >> ---*
 000-DSP-ERR            SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPFILE.
     DISPLAY  FILE-ERR030         UPON    CONS.
     DISPLAY  SEC-NAME            UPON    CONS.
     DISPLAY  DSP-ST              UPON    CONS.
     ACCEPT   IN-DATA             FROM    CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E         0.0      *
****************************************************************
 PROCESS-START          SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     PROGRAM-END    =   "END".
     PERFORM  END-SEC.
     STOP     RUN.
****************************************************************
*             初期処理                                1.0      *
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     JHSHENL1.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
     ACCEPT   SYS-TIME       FROM TIME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     DISPLAY  "*** SSY9001L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*    売上ファイルREAD
     PERFORM  900-OJY-READ.
*
*    初期値セット
     MOVE     ZERO                TO   DENNO1.
     MOVE     ZERO                TO   CNT-DENPYO.
     MOVE     SPACE               TO   END-FLG.
 INIT-EXIT.
     EXIT.
****************************************************************
*             オージョイフル売上ファイル　 READ                *
****************************************************************
 900-OJY-READ           SECTION.
     MOVE     "900-OJY-READ"      TO   S-NAME.
*
     READ     JHSHENL1  AT   END
**************DISPLAY "DDD" UPON CONS
              MOVE     "END"      TO   PROGRAM-END
              GO        TO        900-OJY-READ-EXIT
     END-READ.
*
*    オージョイフル以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  168201
              GO                 TO   900-OJY-READ
     END-IF.
*ブレイクチェック
     IF   LINK-SOKO = SPACE THEN
          IF       DEN-F46   NOT =  LINK-JDATE       OR
                   DEN-F47   NOT =  LINK-JTIME       OR
                   DEN-A10   NOT =  LINK-TORICD
**************DISPLAY "BBB" UPON CONS
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-OJY-READ-EXIT
          END-IF
     ELSE
          IF       DEN-F46   NOT =  LINK-JDATE       OR
                   DEN-F47   NOT =  LINK-JTIME       OR
                   DEN-A10   NOT =  LINK-TORICD      OR
                   DEN-F48   NOT =  LINK-SOKO
**************DISPLAY "CCC" UPON CONS
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-OJY-READ-EXIT
          END-IF
     END-IF.
*店舗ＣＤ範囲チェック
     IF       LINK-STENCD  <=  DEN-A05
     AND      LINK-ETENCD  >=  DEN-A05
              CONTINUE
     ELSE
**************DISPLAY "EEE" UPON CONS
              GO        TO        900-OJY-READ
     END-IF.
*
*納品日範囲チェック
     IF       LINK-SNOUDT  <=  DEN-A09
     AND      LINK-ENOUDT  >=  DEN-A09
              CONTINUE
     ELSE
**************DISPLAY "FFF" UPON CONS
              GO        TO        900-OJY-READ
     END-IF.
*
        ADD      1         TO        READ-CNT.
*
 900-OJY-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
     MOVE          "MAIN-SEC"          TO   S-NAME.
*    ヘッダ項目がブレイク時改ページ
*（店舗Ｃ／部門Ｃ／伝票区分／伝Ｎｏ／取引先Ｃ／発注日／納品日）
     IF       DEN-A05   NOT =  WK-DEN-A05
     OR       DEN-A06   NOT =  WK-DEN-A06
     OR       DEN-A07   NOT =  WK-DEN-A07
     OR       DEN-A03   NOT =  WK-DEN-A03
     OR       DEN-A10   NOT =  WK-DEN-A10
     OR       DEN-A08   NOT =  WK-DEN-A08
     OR       DEN-A09   NOT =  WK-DEN-A09
              MOVE      DEN-A05   TO  WK-DEN-A05
              MOVE      DEN-A06   TO  WK-DEN-A06
              MOVE      DEN-A07   TO  WK-DEN-A07
              MOVE      DEN-A03   TO  WK-DEN-A03
              MOVE      DEN-A10   TO  WK-DEN-A10
              MOVE      DEN-A08   TO  WK-DEN-A08
              MOVE      DEN-A09   TO  WK-DEN-A09
              MOVE      ZERO      TO  SURYOKEI    TSSURYOKEI
                                      GENKINKEI   BAIKINKEI
                                      TSGENKEI    TSBAIKEI
     END-IF.
*
     PERFORM       DENP-WT-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             伝票出力処理                            2.3      *
****************************************************************
 DENP-WT-SEC            SECTION.
     MOVE     "DENP-WT"           TO        S-NAME.
*伝票　ブレイクチェック
     IF       DEN-A03   NOT =               DENNO1
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
              MOVE      DEN-A03   TO        DENNO1
     END-IF.
*
*明細部セット
     MOVE      SPACE     TO       DT1  DT2.
*
*明細１行目
***  商品名（上段）
     MOVE      DEN-A36             TO   DT1-01.
***  訂正伝票時処理
     IF        DEN-F15   NOT =    DEN-F50
***            訂正後原価金額
               COMPUTE  DT1-02 =  DEN-F15 * DEN-F172
***            訂正後売価金額
               COMPUTE  DT1-03 =  DEN-F15 * DEN-F173
     END-IF.
*
*明細２行目
***  商品名（下段）
     MOVE     DEN-A37             TO        DT2-01.
***  商品コード
     MOVE     DEN-A26             TO        DT2-02.
***  数量
     MOVE     DEN-F50             TO        WK-SURYO.
***  数量（整数部）
     MOVE     WK-SURYO1           TO        DT2-031.
***  数量（端数部）
     IF       WK-SURYO2   =      ZERO
              MOVE     ZERO       TO        DT2-032
     ELSE
              MOVE     WK-SURYO2  TO        DT2-042
     END-IF.
*
     COMPUTE  SURYOKEI  =   SURYOKEI  +  DEN-F50.
*    訂正後数量
     IF       DEN-F15   NOT =     DEN-F50
              MOVE      DEN-F15   TO        WK-SURYO
***           訂正後数量（整数部）
              MOVE     WK-SURYO1            TO        DT2-041
***           訂正後数量（端数部）
              IF       WK-SURYO2  =         ZERO
                       MOVE       ZERO      TO        DT2-042
              ELSE
                       MOVE     WK-SURYO2   TO        DT2-042
              END-IF
*
              COMPUTE  TSSURYOKEI  =  TSSURYOKEI  +  DEN-F15
     END-IF.
***  原単価
     MOVE     DEN-F512            TO        WK-GENKA.
***  原単価（整数部）
     MOVE     WK-GENKA1           TO        DT2-051.
***  原単価（端数部）
     IF       WK-GENKA2   =       ZERO
              MOVE     ZERO       TO        DT2-052
     ELSE
              MOVE WK-GENKA2      TO        DT2-052
     END-IF.
***  原価金額
     MOVE     DEN-F521            TO   DT2-06.
***  売価単価
     MOVE     DEN-F513            TO   DT2-07.
***  売価金額
     MOVE     DEN-F522            TO   DT2-08.
*
*原価・売価合計計算
     COMPUTE  GENKINKEI = GENKINKEI + DEN-F521.
     COMPUTE  BAIKINKEI = BAIKINKEI + DEN-F522.
*訂正原価・売価合計計算
***  訂正有時
     IF       DEN-F15   NOT =     DEN-F50
              COMPUTE   TSGENKEI  = DEN-F15 * DEN-F172 + TSGENKEI
              COMPUTE   TSBAIKEI  = DEN-F15 * DEN-F173 + TSBAIKEI
     ELSE
***  訂正無時
              COMPUTE   TSGENKEI  = TSGENKEI + DEN-F521
              COMPUTE   TSBAIKEI  = TSBAIKEI + DEN-F522
     END-IF.
*
*明細１行目出力　　　
     WRITE    PRT-REC   FROM      DT1       AFTER     1.
*明細２行目出力
     WRITE    PRT-REC   FROM      DT2       AFTER     1.
*訂正伝票時処理（明細２行目二重線）
     IF       DEN-F15   NOT = DEN-F50
              WRITE     PRT-REC   FROM   DT21  AFTER     0
     END-IF.
*
     ADD      2                   TO        L-CNT.
*
     PERFORM  900-OJY-READ.
     IF       END-FLG   =         "END"
              PERFORM   TAIL-WT-SEC
     END-IF.
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                        2.3.1    *
****************************************************************
 HEAD-WT-SEC            SECTION.
     MOVE     "HEAD-WT"           TO        S-NAME.
*ヘッダクリア
     MOVE     SPACE               TO   HD1 HD2 HD3.
*
*ヘッダ１行目　
***  Ｅ欄
**   MOVE     DEN-A13             TO    HD1-02.
*
*ヘッダ２行目　
***  社名
     MOVE     DEN-A12             TO    HD2-01.
*
*ヘッダ　３　行目　
***  店名
     MOVE     DEN-A13             TO    HD3-01.
***  店コード
     MOVE     DEN-A05             TO    HD3-02.
***  部門
     MOVE     DEN-A06             TO    HD3-03.
***  伝区
     MOVE     DEN-A07             TO    HD3-04.
***  伝票_
     MOVE     DEN-A03             TO    HD3-05.
***  取引先ＣＤ
     MOVE     DEN-A10             TO    HD3-06.
***  取引先名（下段）
     MOVE     "(ｶﾌﾞ) ｻｶﾀﾉﾀﾈ"      TO    HD3-07.
***  発注日
     MOVE     DEN-A08(1:2)        TO    HD3-081.
     MOVE     DEN-A08(3:2)        TO    HD3-082.
     MOVE     DEN-A08(5:2)        TO    HD3-083.
***  納品日
     MOVE     DEN-A09(1:2)        TO    HD3-091.
     MOVE     DEN-A09(3:2)        TO    HD3-092.
     MOVE     DEN-A09(5:2)        TO    HD3-093.
*
*訂正フラグチェック(TL-WRITE-SECで使用）
     IF       DEN-F53 = 1
              MOVE      1         TO    TS-FLG
              MOVE      NC"○"    TO    HD1-01
     END-IF.
*
*ヘッダ部出力
     WRITE    PRT-REC        FROM      HD1       AFTER    3.
     WRITE    PRT-REC        FROM      HD2       AFTER    3.
     WRITE    PRT-REC        FROM      HD3       AFTER    2.
*
     MOVE     SPACE               TO     PRT-REC.
     WRITE    PRT-REC             AFTER  3.
*
     MOVE     11                  TO     L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                        2.3.2    *
****************************************************************
 TAIL-WT-SEC            SECTION.
     MOVE     "TAIL-WT"           TO        S-NAME.
*テールクリア
     MOVE     SPACE               TO        TL1  TL2.
*テール１行目
***  数量合計
     MOVE   SURYOKEI              TO        WK-SURYO.
***  数量合計（整数部）
     MOVE     WK-SURYO1           TO        TL1-011.
***  数量合計（端数部）
     IF       WK-SURYO2   =       ZERO
              MOVE      ZERO      TO        TL1-012
     ELSE
              MOVE      WK-SURYO2 TO        TL1-012
     END-IF.
*    訂正後数量合計
     IF       GENKINKEI      NOT  =   TSGENKEI
              MOVE      TSSURYOKEI          TO   WK-SURYO
***           訂正後数量合計（整数部）
              MOVE     WK-SURYO1            TO   TL1-021
***           訂正後数量合計（端数部）
              IF       WK-SURYO2  =         ZERO
                       MOVE       ZERO      TO   TL1-022
              ELSE
                       MOVE     WK-SURYO2   TO   TL1-022
              END-IF
     END-IF.
***  原価金額合計
     MOVE   GENKINKEI             TO        TL1-03.
***  売価金額合計
     MOVE   BAIKINKEI             TO        TL1-04.
*テール２行目
***  訂正後合計欄が１マス２バイトである為の処理
***  訂正後原価金額合計
     IF       GENKINKEI      NOT  =   TSGENKEI
              MOVE      ZERO      TO        ZERO-FLG
              MOVE      SPACE     TO        TL2-01
              IF        TSGENKEI  =  ZERO
                MOVE    ZERO      TO        TL2-01(17:1)
              ELSE
                PERFORM  VARYING  I FROM  1  BY  1  UNTIL I > 9
                   COMPUTE  J  =  I  *  2
                   IF  TSGENKEI(I:1)  NOT = ZERO
                       MOVE   1                 TO   ZERO-FLG
                       MOVE   TSGENKEI(I:1)     TO   TL2-01(J:1)
                   ELSE
                       IF      ZERO-FLG   =    1
                               MOVE     ZERO   TO    TL2-01(J:1)
                       END-IF
                   END-IF
                END-PERFORM
              END-IF
***  訂正後売価金額合計
              MOVE      ZERO      TO        ZERO-FLG
              MOVE      SPACE     TO        TL2-02
              IF        TSBAIKEI  =  ZERO
              MOVE      ZERO      TO        TL2-02(17:1)
              ELSE
                PERFORM  VARYING  I FROM  1  BY  1  UNTIL I > 9
                   COMPUTE  J  =  I  *  2
                   IF  TSBAIKEI(I:1)     NOT = ZERO
                       MOVE   1                 TO   ZERO-FLG
                       MOVE   TSBAIKEI(I:1)     TO   TL2-02(J:1)
                   ELSE
                       IF      ZERO-FLG   =    1
                               MOVE     ZERO   TO    TL2-02(J:1)
                       END-IF
                   END-IF
                END-PERFORM
              END-IF
     END-IF.
*
     COMPUTE  CNT-AFTER    =      25   -    L-CNT.
*
*テール部出力
***  テール１行目
     WRITE    PRT-REC        FROM      TL1  AFTER   CNT-AFTER.
***  伝票訂正（二重線）
     IF       GENKINKEI      NOT =     TSGENKEI
              WRITE     PRT-REC   FROM      DT21      AFTER  0
***  テール２行目
     WRITE    PRT-REC        FROM      TL2  AFTER  4
     END-IF.
*
     MOVE     SPACE     TO        PRT-REC.
     WRITE    PRT-REC   AFTER     PAGE.
*
     MOVE     ZERO                TO        GENKINKEI
                                            BAIKINKEI
                                            SURYOKEI
                                            TSGENKEI
                                            TSBAIKEI
                                            TSSURYOKEI
                                            TS-FLG
                                            L-CNT.
*伝票枚数カウント
     ADD      1                   TO        CNT-DENPYO.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             テストプリント　                        2.4      *
****************************************************************
 TEST-PRT-SEC            SECTION.
     MOVE     "TEST-PRT"          TO      S-NAME.
*項目クリア
     MOVE     SPACE               TO   HD1 HD2 HD3
                                       DT1 DT2
                                       TL1.
*テスト印字項目セット
     MOVE   ALL "9"              TO   HD3-081 HD3-082 HD3-083
                                      HD3-091 HD3-092 HD3-093
                                      DT2-041 DT2-042 DT2-06
                                      DT2-08  DT2-031 DT2-032
                                      DT2-051 DT2-052 DT2-07
                                      DT2-08
                                      TL1-011 TL1-021 TL1-03
                                      TL1-012 TL1-022 TL1-04.
     MOVE   ALL "*"              TO   HD1-02
                                      HD2-01
                                      HD3-01  HD3-02  HD3-03
                                      HD3-04  HD3-05  HD3-06
                                      HD3-07
                                      DT1-01
                                      DT2-01  DT2-02.
     MOVE   ALL NC"＊"           TO   HD1-01.
*ヘッダ部
     WRITE    PRT-REC          FROM   HD1        AFTER    3.
     WRITE    PRT-REC          FROM   HD2        AFTER    3.
     WRITE    PRT-REC          FROM   HD3        AFTER    2.
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  3.
*ボディ部
     PERFORM   VARYING   L-CNT      FROM  1  BY  1
                                        UNTIL   L-CNT     >  6
              WRITE  PRT-REC   FROM   DT1       AFTER     1
              WRITE  PRT-REC   FROM   DT2       AFTER     1
     END-PERFORM.
*テール部
     WRITE    PRT-REC          FROM   TL1       AFTER     2.

     CLOSE    PRTFILE.
     OPEN     OUTPUT    PRTFILE.
     MOVE     ZERO      TO        L-CNT.
*
 TEST-PRT-EXIT.
     EXIT.
****************************************************************
*             終了処理                                3.0      *
****************************************************************
 END-SEC                SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*
     CLOSE    JHSHENL1   PRTFILE  DSPFILE.
*
 END-EXIT.
     EXIT.
