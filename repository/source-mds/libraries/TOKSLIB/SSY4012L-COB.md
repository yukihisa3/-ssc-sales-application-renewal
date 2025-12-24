# SSY4012L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY4012L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム                  *
*    業務名　　　　　　　：　セリ配送発注集計表                *
*    モジュール名　　　　：　セリ配送発注集計表出力            *
*    作成日／更新日　　　：　2000/03/01                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　                                  *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION          DIVISION.
****************************************************************
 PROGRAM-ID.             SSY4012L.
 AUTHOR.                 NAV.
 DATE-WRITTEN.           2000.03.01.
****************************************************************
 ENVIRONMENT             DIVISION.
****************************************************************
 CONFIGURATION           SECTION.
 SOURCE-COMPUTER.        FACOM.
 OBJECT-COMPUTER.        FACOM.
 SPECIAL-NAMES.
         CONSOLE         IS             CONS.
*
 INPUT-OUTPUT            SECTION.
 FILE-CONTROL.
*    配送伝票データ
     SELECT   INFILE       ASSIGN       TO    SHWHISF
                           ORGANIZATION       SEQUENTIAL
                           ACCESS  MODE       SEQUENTIAL
                           FILE    STATUS     IN-STATUS.
*    店舗マスタ
*** 97/03/18 S ***
***  SELECT   HTENMS       ASSIGN     TO      DA-01-VI-TENMS2
***                        ORGANIZATION       INDEXED
***                        ACCESS     MODE    SEQUENTIAL
***                        RECORD     KEY     TEN-F52  TEN-F80
***                                           TEN-F81  TEN-F82
     SELECT   HTENMS       ASSIGN     TO      DA-01-VI-TENMS1
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    RANDOM
                           RECORD     KEY     TEN-F52  TEN-F011
*** 97/03/18 E ***
                           FILE    STATUS     TEN-STATUS.
*    条件ファイル
     SELECT   HJYOKEN      ASSIGN     TO      DA-01-VI-JYOKEN1
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    RANDOM
                           RECORD     KEY     JYO-F01  JYO-F02
                           FILE    STATUS     JYO-STATUS.
*    得意先マスタ
     SELECT   HTOKMS       ASSIGN     TO      DA-01-VI-TOKMS2
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    RANDOM
                           RECORD     KEY     TOK-F01
                           FILE    STATUS     TOK-STATUS.
*    プリントファイル
     SELECT  PRTF      ASSIGN    TO             GS-PRTF
                       DESTINATION              "PRT"
                       FORMAT                   PRT-FORM
                       GROUP                    PRT-GRP
                       PROCESSING               PRT-PROC
                       UNIT CONTROL             PRT-CTL
                       FILE STATUS              PRT-STATUS.
*** 97/03/18 S ***
*    店舗別ルートマスタ
     SELECT   HTENRMS      ASSIGN     TO      DA-01-VI-TENRMS2
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    SEQUENTIAL
                           RECORD     KEY     TER-F01  TER-F02
                                              TER-F04  TER-F05
                                              TER-F03
                           FILE    STATUS     TER-STATUS.
*** 97/03/18 E ***
*=============================================================*
 DATA                    DIVISION.
*=============================================================*
 FILE                    SECTION.
*    配送伝票データ
 FD  INFILE
     BLOCK       CONTAINS   1        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHWHISF   OF        XFDLIB
     JOINING     IN        AS        PREFIX.
*    店舗マスタ
 FD  HTENMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*    条件ファイル
 FD  HJYOKEN
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*    得意先マスタ
 FD  HTOKMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*    帳票ファイル
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY40121  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
*** 97/03/18 S ***
*    店舗別ルートマスタ
 FD  HTENRMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENRMS   OF        XFDLIB
     JOINING     TER       AS        PREFIX.
*** 97/03/18 E ***
*
*=============================================================*
 WORKING-STORAGE          SECTION.
*=============================================================*
*    制御領域
 01  STATUS-AREA.
     03  IN-STATUS                PIC  X(02).
     03  TEN-STATUS               PIC  X(02).
     03  JYO-STATUS               PIC  X(02).
     03  TOK-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
*** 97/03/18 S ***
     03  TER-STATUS               PIC  X(02).
*** 97/03/18 E ***
*    ＦＯＲＭ制御領域
 01  PRT-FORM                    PIC  X(08).
 01  PRT-PROC                    PIC  X(02).
 01  PRT-GRP                     PIC  X(08).
 01  PRT-CTL.
     03  PRT-CNTRL               PIC  X(04).
     03  PRT-STR-PG              PIC  X(02).
*    フラグエリア
 01  FLG-AREA.
     03  FLG-END                  PIC  X(03)  VALUE  SPACE.
     03  FLG-READ                 PIC  X(03)  VALUE  SPACE.
     03  FLG-TK                   PIC  X(02)  VALUE  SPACE.
*    退避エリア
 01  SAV-AREA.
     03  WK-NOUDATE               PIC  9(08)  VALUE  ZERO.
     03  WK-SYOCD                 PIC  X(13)  VALUE  ZERO.
     03  WK-SYUKA                 PIC  X(02)  VALUE  SPACE.
     03  WK-RUTO                  PIC  9(02)  VALUE  ZERO.
     03  WK-SERI1                 PIC S9(09)  PACKED-DECIMAL.
     03  WK-SERI2                 PIC S9(09)  PACKED-DECIMAL.
     03  PAGE-CNT                 PIC  9(04)  VALUE  ZERO.
     03  TATE                     PIC  9(02)  VALUE  ZERO.
     03  YOKO                     PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(02)  VALUE  ZERO.
     03  IY                       PIC  9(02)  VALUE  ZERO.
     03  IZ                       PIC  9(01)  VALUE  ZERO.
     03  WK-HENKAN                PIC  N(01)  VALUE  SPACE.
     03  CHK-FLG                  PIC  9(01)  VALUE  ZERO.
     03  WK-GK-SURYO              PIC  9(06)  VALUE  ZERO.
     03  WK-DATE                  PIC  9(01)  VALUE  ZERO.
     03  READ-CNT                 PIC  9(06)  VALUE  ZERO.
*    ルート毎店舗情報ワークエリア
 01  WK-TENPO.
     03  TENPO                    OCCURS      13.
         05  TENCD                PIC  9(03)  VALUE  ZERO.
         05  TENMEI               PIC  N(03)  VALUE  SPACE.
*    ルート番号退避
 01  WK-KAKUNOU.
     03  WK-RUTONO                PIC  9(02)  VALUE  ZERO.
*    エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*    漢字変換
 01  WK-KANJI.
     03  KANJI                    PIC  N(10)  VALUE
         NC"１２３４５６７８９０".
 01  WK-KANJIR                    REDEFINES   WK-KANJI.
     03  WK-SU                    OCCURS      10.
         05  SU                   PIC  N(01).
*    システム日付
 01  WRK-DATE.
     03  WRK-DATE1                PIC  9(02)  VALUE  ZERO.
     03  WRK-DATE2                PIC  9(06)  VALUE  ZERO.
*    漢字ワークエリア
 01  WK-HENKAN-KANJI.
     03  WK-HENK1.
         05  WK-HENK11            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK12            PIC  N(01)  VALUE  SPACE.
     03  WK-HENK2.
         05  WK-HENK21            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK22            PIC  N(01)  VALUE  SPACE.
     03  WK-HENK3.
         05  WK-HENK31            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK32            PIC  N(01)  VALUE  SPACE.
*    合計金額格納エリア
 01  GOKEI-AREA.
     03  GOKEI-GENKA              OCCURS      13     TIMES.
         05  GENKEI               PIC S9(09).
     03  GOKEI-BAIKA              OCCURS      13     TIMES.
         05  BAIKEI               PIC S9(09).
*    商品名編集エリア
 01  SYOHIN-AREA.
     03  SYOHIN-1.
         05  WK-SYOHIN-11         PIC  X(14).
         05  WK-SYOHIN-12         PIC  X(01).
     03  SYOHIN-2.
         05  WK-SYOHIN-21         PIC  X(01).
         05  WK-SYOHIN-22         PIC  X(14).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-PG-ID            PIC  X(08)  VALUE  "SSY4012L".
         05  FILLER               PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID            PIC  X(08).
         05  FILLER               PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD             PIC  X(02).
         05  FILLER               PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                 SECTION.
*01  LINK-SYUKKA                  PIC  X(02).
*============================================================*
 PROCEDURE               DIVISION.
*============================================================*
 DECLARATIVES.
*    プリントファイル
 FILEERR-SEC1           SECTION.
     USE AFTER EXCEPTION PROCEDURE   PRTF.
     MOVE      "PRTF"           TO   ERR-FL-ID.
     MOVE      PRT-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　配送伝票データ
 FILEERR-SEC2           SECTION.
     USE AFTER EXCEPTION PROCEDURE   INFILE.
     MOVE      "INFILE"         TO   ERR-FL-ID.
     MOVE      IN-STATUS        TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　店舗マスタ
 FILEERR-SEC3           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HTENMS.
     MOVE      "HTENMS"         TO   ERR-FL-ID.
     MOVE      TEN-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　条件ファイル
 FILEERR-SEC4           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HJYOKEN.
     MOVE      "HJYOKEN"        TO   ERR-FL-ID.
     MOVE      JYO-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　得意先マスタ
 FILEERR-SEC4           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HTOKMS.
     MOVE      "HTOKMS"         TO   ERR-FL-ID.
     MOVE      TOK-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*** 97/03/18 S ***
*　　店舗別ルートマスタ
 FILEERR-SEC6           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HTENRMS.
     MOVE      "HTENRMS"        TO   ERR-FL-ID.
     MOVE      TER-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*** 97/03/18 E ***
 END     DECLARATIVES.
*
*============================================================*
*　　ゼネラル処理　　　　　　　　　　　　  構造_0.0         *
*============================================================*
 CONTROL-SEC             SECTION.
     MOVE     "COTROL-SEC"        TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL    FLG-END  =  "END".
     PERFORM  END-SEC.
     STOP     RUN.
*
 CONTROL-EXIT.
     EXIT.
*============================================================*
*　　初期処理　　　　　　　　　　　　　　  構造_1.0         *
*============================================================*
 INIT-SEC                SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*    使用ファイル　ＯＰＥＮ
*** 97/03/18 S ***
***  OPEN     INPUT      INFILE HJYOKEN HTOKMS.
     OPEN     INPUT      INFILE HJYOKEN HTOKMS  HTENMS.
*** 97/03/18 E ***
     OPEN     OUTPUT     PRTF.
*    システム日付の取得
*システム日付・時刻の取得
     ACCEPT   WRK-DATE2         FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WRK-DATE2           TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WRK-DATE.
*    プリントエリア初期化
     MOVE     SPACE      TO       PRT-FSY40121.
     MOVE     ZERO       TO       READ-CNT.
     MOVE     ZERO       TO       GOKEI-AREA.
     MOVE     SPACE      TO       FLG-TK.
*    配送伝票データ初期ＲＥＡＤ
     PERFORM  HIDENJNL-RD-SEC.
     MOVE     IN-F112    TO       WK-NOUDATE.
     MOVE     IN-F25     TO       WK-SYOCD.
     MOVE     IN-F98     TO       WK-RUTO.
     MOVE     IN-F08     TO       WK-SYUKA.
     MOVE     IN-F19     TO       WK-SERI1.
     MOVE     IN-F20     TO       WK-SERI2.
     MOVE     1          TO       TATE.
     PERFORM  RUTO-SET-SEC.
     IF       FLG-END    =        "END"
      DISPLAY NC"指定された出荷場所のデータが存在しません。"
              UPON      CONS
     END-IF.
*
 INIT-EXT.
     EXIT.
*============================================================*
*　　配送伝票データ読込み　　　　　　　　  構造_            *
*============================================================*
 HIDENJNL-RD-SEC       SECTION.
     MOVE     "HIDENJNL-RD-SEC"   TO   S-NAME.
*    配送伝票データ読込み
     READ     INFILE   AT  END
              IF READ-CNT > ZERO
                 MOVE      WK-GK-SURYO     TO     PRT-SOGK(TATE)
                 MOVE      "ON"            TO     FLG-TK
                 PERFORM   SYUKEI-WT-SEC
              END-IF
              MOVE     "END"      TO       FLG-END
              GO                  TO       HIDENJNL-RD-EXIT
     END-READ.
 READ010.
**** パラメタの出荷場所以外は読飛し
**** IF       LINK-SYUKKA  NOT =  IN-F08
****          GO                  TO       HIDENJNL-RD-SEC
**** END-IF.
 READ020.
*    ルートコード＝空白時，読飛し
     IF       IN-F08       =      SPACE
     OR       IN-F98       =      ZERO
     OR       IN-F99       =      ZERO
              GO                  TO       HIDENJNL-RD-SEC
     END-IF.
*    対象データ件数カウント
     ADD      1                   TO       READ-CNT.
*
 HIDENJNL-RD-EXIT.
     EXIT.
*============================================================*
*　　メイン処理　　　　　　　　　　　　　  構造_2.0         *
*============================================================*
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
*    ブレイクチェック（納品日，ルート）
     IF       WK-NOUDATE  NOT =  IN-F112
     OR       WK-RUTO     NOT =  IN-F98
     OR       WK-SYUKA    NOT =  IN-F08
              MOVE  WK-GK-SURYO  TO  PRT-SOGK(TATE)
              MOVE  "ON"         TO  FLG-TK
              PERFORM            SYUKEI-WT-SEC
              MOVE  IN-F112      TO  WK-NOUDATE
              MOVE  IN-F25       TO  WK-SYOCD
              MOVE  IN-F98       TO  WK-RUTO
              MOVE  IN-F08       TO  WK-SYUKA
              MOVE  IN-F19       TO  WK-SERI1
              MOVE  IN-F20       TO  WK-SERI2
              MOVE  1            TO  TATE
              MOVE  ZERO         TO  WK-GK-SURYO
              PERFORM            RUTO-SET-SEC
     END-IF.
*    商品コードブレイク時
     IF       WK-SYOCD    NOT =  IN-F25
     OR       WK-SERI1    NOT =  IN-F19
     OR       WK-SERI2    NOT =  IN-F20
              MOVE  IN-F25       TO  WK-SYOCD
              MOVE  IN-F19       TO  WK-SERI1
              MOVE  IN-F20       TO  WK-SERI2
              MOVE  WK-GK-SURYO  TO  PRT-SOGK(TATE)
              MOVE  ZERO         TO  WK-GK-SURYO
              ADD   1            TO  TATE
*    同頁内で商品が２８アイテム以上の時，次頁へ改頁
              IF    TATE  >  28
                    PERFORM          SYUKEI-WT-SEC
                    MOVE  IN-F112    TO  WK-NOUDATE
                    MOVE  IN-F25     TO  WK-SYOCD
                    MOVE  IN-F98     TO  WK-RUTO
                    MOVE  IN-F08     TO  WK-SYUKA
                    MOVE  IN-F19     TO  WK-SERI1
                    MOVE  IN-F20     TO  WK-SERI2
                    MOVE  1          TO  TATE
                    MOVE  ZERO       TO  WK-GK-SURYO
                    PERFORM          RUTO-SET-SEC
              END-IF
     END-IF.
*    項目セット処理へ
     PERFORM  DATA-SET-SEC.
*    配送伝票データ読込み
     PERFORM  HIDENJNL-RD-SEC.
*
 MAIN-EXT.
     EXIT.
*============================================================*
*　　終了処理　　　　　　　　　　　　　　  構造_3.0         *
*============================================================*
 END-SEC                 SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*    使用ファイルＣＬＯＳＥ
     CLOSE                        INFILE HTOKMS HJYOKEN
*** 97/03/18 S ***
                                  HTENMS
*** 97/03/18 E ***
                                  PRTF.
*    終了メッセージ
     DISPLAY "************************" UPON  CONS.
     DISPLAY "*   ｾﾘ配送発注集計表   *" UPON  CONS.
     DISPLAY "*  ｼｭﾂﾘｮｸ ｹﾝｽｳ = " PAGE-CNT "  *"  UPON  CONS.
     DISPLAY "************************" UPON  CONS.
*
 END-EXT.
     EXIT.
*============================================================*
*　　ルートセット処理　　　                構造_1.1         *
*============================================================*
 RUTO-SET-SEC          SECTION.
     MOVE     "RUTO-SET-SEC"      TO   S-NAME.
*** 97/03/18 START COMMENT ***********************************
*    店舗マスタのＯＰＥＮ
***  OPEN     INPUT    HTENMS.
***  MOVE     ZERO          TO    IX.
***  INITIALIZE                   WK-TENPO.
*    店舗マスタスタート
**** DISPLAY "IN-F01      = " IN-F01      UPON CONS.
**** DISPLAY "LINK-SYUKKA = " LINK-SYUKKA UPON CONS.
**** DISPLAY "IN-F98      = " IN-F98      UPON CONS.
***  MOVE     IN-F01        TO    TEN-F52.
***  MOVE     LINK-SYUKKA   TO    TEN-F80.
***  MOVE     IN-F98        TO    TEN-F81.
***  MOVE     ZERO          TO    TEN-F82.
***  START    HTENMS  KEY  IS  >= TEN-F52 TEN-F80 TEN-F81 TEN-F82
***           INVALID
***           DISPLAY NC"量販店が存在しません！！" TEN-F52
***                                                UPON    CONS
***           DISPLAY NC"店舗マスタメンテで確認してください！！"
***                                                UPON    CONS
***           STOP  RUN
***  END-START.
*    店舗マスタのＳＥＱ．読み
*RUTO010.
***  READ    HTENMS    AT  END
***          GO        TO       RUTO020
***  END-READ.
*    ブレイクチェック
***  IF    IN-F01      NOT =    TEN-F52
***  OR    LINK-SYUKKA NOT =    TEN-F80
***  OR    IN-F98      NOT =    TEN-F81
***  OR    IX          >        13
***        GO          TO       RUTO020
***  ELSE
***        ADD         1        TO       IX
********** DISPLAY "TEN-F011 = " TEN-F011 UPON CONS
***        MOVE    TEN-F011     TO       TENCD(IX)
***        MOVE    TEN-F03      TO       TENMEI(IX)
***        MOVE    TEN-F81      TO       WK-RUTONO
***        GO                   TO       RUTO010
***  END-IF.
*
*RUTO020.
***  CLOSE         HTENMS.
*** 97/03/18 END COMMENT**************************************
***
*** 97/03/18 START *******************************************
*    店舗別ルートマスタのＯＰＥＮ
     OPEN     INPUT    HTENRMS.
     MOVE     ZERO          TO    IX.
     INITIALIZE                   WK-TENPO.
*    店舗別ルートマスタスタート
     MOVE     IN-F01        TO    TER-F01.
     MOVE     IN-F08        TO    TER-F02.
     MOVE     IN-F98        TO    TER-F04.
     MOVE     ZERO          TO    TER-F05.
     MOVE     ZERO          TO    TER-F03.
     START    HTENRMS  KEY  IS  >= TER-F01 TER-F02 TER-F04
                                   TER-F05 TER-F03
              INVALID
              DISPLAY NC"量販店が存在しません！！" TER-F01
                                                   UPON    CONS
              DISPLAY NC"出荷場所＝"  TER-F02      UPON    CONS
              DISPLAY NC"ルート　＝"  TER-F04      UPON    CONS
              DISPLAY NC"店舗別ルートマスタメンテで"
                      NC"確認してください！！"
                                                   UPON    CONS
              STOP  RUN
     END-START.
*    店舗別ルートマスタのＳＥＱ．読み
 RUTO010.
     READ    HTENRMS   AT  END
             GO        TO       RUTO020
     END-READ.
*    ブレイクチェック
     IF    IN-F01      NOT =    TER-F01
     OR    IN-F08      NOT =    TER-F02
     OR    IN-F98      NOT =    TER-F04
     OR    IX          >=       13
           GO          TO       RUTO020
     ELSE
           ADD         1        TO       IX
           MOVE    TER-F03      TO       TENCD(IX)
*          店舗名略名取得
           MOVE    TER-F01      TO       TEN-F52
           MOVE    TER-F03      TO       TEN-F011
           READ    HTENMS
              INVALID
                   DISPLAY  "(SSY4012L) HTENMS INV KEY="
                       TEN-F52 "," TEN-F011    UPON CONS
                   MOVE      SPACE     TO   TENMEI(IX)
              NOT INVALID
                   MOVE      TEN-F03   TO   TENMEI(IX)
           END-READ
           MOVE    TER-F04      TO       WK-RUTONO
           GO                   TO       RUTO010
     END-IF.
*
 RUTO020.
     CLOSE         HTENRMS.
*** 97/03/18 END *********************************************
*
 RUTO-SET-EXIT.
     EXIT.
*============================================================*
*    配送発注集計表出力処理                構造_2.1         *
*============================================================*
 SYUKEI-WT-SEC         SECTION.
     MOVE     "SYUKEI-WT-SEC"     TO   S-NAME.
*    ページ　カウントアップ
     ADD      1        TO        PAGE-CNT.
     MOVE   PAGE-CNT   TO        PRT-HDPAGE.
*    システム日付セット
     MOVE   WRK-DATE    TO       PRT-HDDATE.
*    納品日漢字変換
     PERFORM VARYING IZ FROM 3 BY 1 UNTIL IZ > 8
             PERFORM KANJI-HENKAN-SEC
     END-PERFORM.
*    変換文字転送
     MOVE   WK-HENK1   TO        PRT-HEDNEN.
     MOVE   WK-HENK2   TO        PRT-HEDMON.
     MOVE   WK-HENK3   TO        PRT-HEDDAT.
*    量販店名の取得
     MOVE   IN-F01     TO        TOK-F01.
     READ   HTOKMS     INVALID
            MOVE      ALL NC"＊" TO   PRT-RYOMEI
     NOT    INVALID
            MOVE      TOK-F03    TO   PRT-RYOMEI
     END-READ.
*    出荷場所名の取得
     MOVE   20         TO        JYO-F01.
     MOVE   WK-SYUKA   TO        JYO-F02.
     READ   HJYOKEN    INVALID
            MOVE      ALL NC"＊" TO   PRT-SYUKKA
     NOT    INVALID
            MOVE      JYO-F03    TO   PRT-SYUKKA
     END-READ.
*    ルート番号転送
     MOVE   WK-RUTONO            TO   PRT-RUTONO.
*    ルート順で店舗コード，店名転送
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 13
             IF       TENCD(IX)  NOT =   ZERO
                      MOVE     TENCD(IX)   TO   PRT-TENCD(IX)
                      MOVE     TENMEI(IX)  TO   PRT-TENMEI(IX)
             END-IF
     END-PERFORM.
*    店舗毎→原価／売価／値入率セット
     IF      FLG-TK  =  "ON"
             PERFORM GOKEI-SET-SEC
             MOVE    ZERO         TO   GOKEI-AREA
             MOVE    SPACE        TO   FLG-TK
     END-IF.
*    印字制御項目セット
     MOVE     SPACE               TO   PRT-PROC.
     MOVE     SPACE               TO   PRT-CTL.
     MOVE     SPACE               TO   PRT-FORM.
     MOVE    "FSY40121"           TO   PRT-FORM.
     MOVE    "SCREEN"             TO   PRT-GRP.
*    配送発注集計表出力
 SYUKEI010.
     WRITE    PRT-FSY40121.
*    プリントエリア初期化
     MOVE     SPACE               TO   PRT-FSY40121.
*
 SYUKEI-WT-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造_2.2         *
*============================================================*
 DATA-SET-SEC            SECTION.
     MOVE     "DATA-SET-SEC"      TO   S-NAME.
*    合計レコード時
     IF      IN-F99  =  99
             MOVE      IN-F15  TO  PRT-GOKEI(TATE)
             GO                TO  DATA-SET-EXIT
     END-IF.
*    サカタコード
     MOVE    IN-F1411    TO        PRT-SKATCD(TATE).
*    ケーヨーコード
     MOVE    IN-F25      TO        PRT-KEIYCD(TATE).
*    商品名１，２
     MOVE    IN-F1421    TO        SYOHIN-1.
     MOVE    SPACE       TO        WK-SYOHIN-12.
     MOVE    IN-F1422    TO        WK-SYOHIN-22.
     MOVE    IN-F1421(15:1) TO     WK-SYOHIN-21.
     MOVE    SYOHIN-1    TO        PRT-SYOHI1(TATE).
     MOVE    SYOHIN-2    TO        PRT-SYOHI2(TATE).
*    規格
     MOVE    IN-F26      TO        PRT-KIKAKU(TATE).
*    サイズ
     MOVE    IN-F27      TO        PRT-SIZE(TATE).
*    入数
     MOVE    IN-F171     TO        PRT-IRISU(TATE).
*    原価
     MOVE    IN-F172     TO        PRT-GENKA(TATE).
*    売価
     MOVE    IN-F173     TO        PRT-BAIKA(TATE).
*    ルート順位置へセット
     MOVE    ZERO        TO       CHK-FLG.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX      > 13
                                       OR CHK-FLG = 1
        IF  IN-F07  =  TENCD(IX)
            MOVE  IX      TO  YOKO
************MOVE  IN-F15  TO  PRT-HATSU(TATE YOKO)
            ADD   IN-F15  TO  PRT-HATSU(TATE YOKO)
*//* 96/04/15  追加 START （原価，売価計算→ワークへセット）
************COMPUTE GENKEI(YOKO) =
************   ( PRT-HATSU(TATE YOKO) * IN-F172 ) + GENKEI(YOKO)
************COMPUTE BAIKEI(YOKO) =
************   ( PRT-HATSU(TATE YOKO) * IN-F173 ) + BAIKEI(YOKO)
            COMPUTE GENKEI(YOKO) =
               ( IN-F15 * IN-F172 ) + GENKEI(YOKO)
            COMPUTE BAIKEI(YOKO) =
               ( IN-F15 * IN-F173 ) + BAIKEI(YOKO)
*****DISPLAY "GENKEI = " GENKEI(YOKO) UPON CONS
*****DISPLAY "BAIKEI = " BAIKEI(YOKO) UPON CONS
*//* 96/04/15  追加 END
            MOVE  1       TO  CHK-FLG
        END-IF
     END-PERFORM.
*    ルート計計算
     ADD     IN-F15    TO  WK-GK-SURYO.
*
 DATA-SET-EXIT.
     EXIT.
*============================================================*
*　　漢字変換処理　　　　　                構造_2.1.1       *
*============================================================*
 KANJI-HENKAN-SEC      SECTION.
     MOVE     "KANJI-HENKAN-SEC"  TO   S-NAME.
*    数字を漢字に変換（ワーク変換テーブルにより）
     MOVE    WK-NOUDATE(IZ:1)          TO     WK-DATE.
     IF   (  WK-DATE  =  ZERO  )
     AND  (  IZ       =  3  OR  5  OR  7  )
             MOVE  SPACE               TO     WK-HENKAN
     ELSE
             MOVE  WK-DATE             TO     IY
             IF    IY  =  ZERO
                   MOVE   10           TO     IY
             END-IF
             MOVE  SU(IY)              TO     WK-HENKAN
     END-IF.
*    変換後転送
     EVALUATE IZ
         WHEN 3
              MOVE WK-HENKAN           TO     WK-HENK11
         WHEN 4
              MOVE WK-HENKAN           TO     WK-HENK12
         WHEN 5
              MOVE WK-HENKAN           TO     WK-HENK21
         WHEN 6
              MOVE WK-HENKAN           TO     WK-HENK22
         WHEN 7
              MOVE WK-HENKAN           TO     WK-HENK31
         WHEN 8
              MOVE WK-HENKAN           TO     WK-HENK32
     END-EVALUATE.
*
 KANJI-HENKAN-EXIT.
     EXIT.
*============================================================*
*　　合計セット（店舗毎）　                構造_2.1.2       *
*============================================================*
 GOKEI-SET-SEC         SECTION.
     MOVE     "GOKEI-SET-SEC"     TO   S-NAME.
*   テーブルよりプリントエリアへデータセット
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 13
        IF   GENKEI(IX)  NOT =  ZERO
             COMPUTE PRT-GENKEI(IX) = GENKEI(IX) / 1000
        END-IF
        IF   BAIKEI(IX)  NOT =  ZERO
             COMPUTE PRT-BAIKEI(IX) = BAIKEI(IX) / 1000
        END-IF
        IF   GENKEI(IX)  NOT =  ZERO
             COMPUTE PRT-NEIRE (IX) ROUNDED =
                ( 1 - ( GENKEI(IX) / BAIKEI(IX) ))  * 100
        END-IF
     END-PERFORM.
*
 GOKEI-SET-EXIT.
     EXIT.

```
