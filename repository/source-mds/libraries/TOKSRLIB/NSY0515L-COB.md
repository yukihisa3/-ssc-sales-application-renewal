# NSY0515L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY0515L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭ仕入先統合　　　　　　　　　*
*    業務名　　　　　　　：　センター納品対応　　　　　　　　　*
*    モジュール名　　　　：　荷個数明細書出力（店直Ｓ経由）　  *
*    作成日／更新日　　　：　21/02/18                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　                                  *
*    再利用ＰＧ　　　　　：                                    *
**履歴**********************************************************
*    2021/02/18  高橋　　新規作成（ＳＳＹ９２０６Ｌコピー）　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION          DIVISION.
****************************************************************
 PROGRAM-ID.             NSY0515L.
 AUTHOR.                 NAV
 DATE-WRITTEN.           18/02/19.
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
*    センター納品データ
     SELECT   DDCENTF      ASSIGN     TO    DA-01-VI-DDCENTL3
                           ORGANIZATION     INDEXED
                           ACCESS     MODE  SEQUENTIAL
                           RECORD     KEY   DKC-F01   DKC-F02
                                            DKC-F03   DKC-F04
                                            DKC-F06   DKC-F18
                                            DKC-F05   DKC-F19
                                            DKC-F07
                           FILE    STATUS   DKC-STATUS.
*    店舗マスタ
     SELECT   HTENMS       ASSIGN     TO      DA-01-VI-TENMS1
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    RANDOM
                           RECORD     KEY     TEN-F52  TEN-F011
                           FILE    STATUS     TEN-STATUS.
*    プリントファイル
     SELECT  PRTF      ASSIGN    TO             GS-PRTF
                       DESTINATION              "PRT"
                       FORMAT                   PRT-FORM
                       GROUP                    PRT-GRP
                       PROCESSING               PRT-PROC
                       UNIT CONTROL             PRT-CTL
                       FILE STATUS              PRT-STATUS.
*#2019/03/19 NAV ST
*発注種別変換マスタ
     SELECT   DCMHSBL1  ASSIGN    TO        DA-01-VI-DCMHSBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HSB-F01  HSB-F02
                        FILE  STATUS   IS   HSB-STATUS.
*#2019/03/19 NAV ED
*=============================================================*
 DATA                    DIVISION.
*=============================================================*
 FILE                    SECTION.
*    センター納品データ
 FD  DDCENTF            LABEL RECORD   IS   STANDARD.
     COPY     DDCENTF   OF        XFDLIB
              JOINING   DKC       PREFIX.
*    店舗マスタ
 FD  HTENMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*    帳票ファイル
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY05101  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
*#2019/03/19 NAV ST
******************************************************************
*    発注種別変換マスタ
******************************************************************
 FD  DCMHSBL1           LABEL RECORD   IS   STANDARD.
     COPY     DCMHSBF   OF        XFDLIB
              JOINING   HSB       PREFIX.
*
*#2019/03/19 NAV ED
*
*=============================================================*
 WORKING-STORAGE          SECTION.
*=============================================================*
*    制御領域
 01  STATUS-AREA.
     03  DKC-STATUS               PIC  X(02).
     03  TEN-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
*#2019/03/19 NAV ST
     03  HSB-STATUS               PIC  X(02).
*#2019/03/19 NAV ED
*    ＦＯＲＭ制御領域
 01  PRT-FORM                     PIC  X(08).
 01  PRT-PROC                     PIC  X(02).
 01  PRT-GRP                      PIC  X(08).
 01  PRT-CTL.
     03  PRT-CNTRL                PIC  X(04).
     03  PRT-STR-PG               PIC  X(02).
*    フラグエリア
 01  FLG-AREA.
     03  FLG-END                  PIC  X(03)  VALUE  SPACE.
     03  FLG-READ                 PIC  X(03)  VALUE  SPACE.
*    退避エリア
 01  SAV-AREA.
     03  WK-NOUDATE               PIC  9(08)  VALUE  ZERO.
     03  WK-TORIHIKI              PIC  9(08)  VALUE  ZERO.
     03  WK-TENPO                 PIC  9(05)  VALUE  ZERO.
     03  WK-TENPO-N               PIC  N(15)  VALUE  SPACE.
     03  WK-HACKBN                PIC  X(02)  VALUE  SPACE.
     03  PAGE-CNT                 PIC  9(04)  VALUE  ZERO.
     03  TATE                     PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(02)  VALUE  ZERO.
     03  IY                       PIC  9(02)  VALUE  ZERO.
     03  IZ                       PIC  9(01)  VALUE  ZERO.
     03  WK-GK-SURYO              PIC  9(07)  VALUE  ZERO.
     03  WK-DATE                  PIC  9(01)  VALUE  ZERO.
     03  READ-CNT                 PIC  9(06)  VALUE  ZERO.
*#2019/03/19 NAV ST
     03  DCMHSBL1-INV-FLG         PIC  X(03)  VALUE  SPACE.
*#2019/03/19 NAV ED
*    エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*    システム日付
 01  WRK-DATE.
     03  WRK-DATE1                PIC  9(02)  VALUE  ZERO.
     03  WRK-DATE2                PIC  9(06)  VALUE  ZERO.
*
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-PG-ID            PIC  X(08)  VALUE  "NSY0515L".
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
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUST             PIC   9(08).
 01  PARA-NOUED             PIC   9(08).
 01  PARA-TENST             PIC   9(05).
 01  PARA-TENED             PIC   9(05).
*============================================================*
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUST
                                       PARA-NOUED
                                       PARA-TENST
                                       PARA-TENED.
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
*　　センター納品データ
 FILEERR-SEC2           SECTION.
     USE AFTER EXCEPTION PROCEDURE   DDCENTF.
     MOVE      "DDCENTF"        TO   ERR-FL-ID.
     MOVE      DKC-STATUS       TO   ERR-STCD.
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
*　　発注種別変換マスタ
 FILEERR-SEC4           SECTION.
     USE AFTER EXCEPTION PROCEDURE   DCMHSBL1.
     MOVE      "DCMHSBL1"       TO   ERR-FL-ID.
     MOVE      HSB-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
*
*============================================================*
*　　ゼネラル処理　　　　　　　　　　　　  構造■0.0         *
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
*　　初期処理　　　　　　　　　　　　　　  構造■1.0         *
*============================================================*
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*    使用ファイル　ＯＰＥＮ
     OPEN     INPUT     DDCENTF  HTENMS.
     OPEN     OUTPUT    PRTF.
*#2019/03/19 NAV ST
     OPEN     INPUT     DCMHSBL1.
*#2019/03/19 NAV ED
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
     MOVE     LINK-OUT-YMD        TO   WRK-DATE.
*    プリントエリア初期化
     MOVE     SPACE          TO   PRT-FSY05101.
     MOVE     ZERO           TO   READ-CNT.
*
*    センター納品データスタート
     MOVE     SPACE          TO   DKC-REC.
     INITIALIZE                   DKC-REC.
     MOVE     PARA-JDATE     TO   DKC-F01.
     MOVE     PARA-JTIME     TO   DKC-F02.
     MOVE     PARA-TORICD    TO   DKC-F03.
     MOVE     PARA-SOKO      TO   DKC-F04.
*****MOVE     PARA-NOUST     TO   DKC-F05.
     START    DDCENTF   KEY  >=   DKC-F01   DKC-F02
                                  DKC-F03   DKC-F04
                                  DKC-F06   DKC-F18
                                  DKC-F05   DKC-F19
                                  DKC-F07
         INVALID   KEY
              MOVE    "END"  TO   FLG-END
              DISPLAY NC"＃＃出力対象無し！！＃＃" UPON CONS
              GO   TO   INIT-EXT
     END-START.
*    センター納品データ初期ＲＥＡＤ
     PERFORM  DDCENTF-RD-SEC.
     MOVE     DKC-F05    TO       WK-NOUDATE.
     MOVE     DKC-F06    TO       WK-TENPO.
     MOVE     DKC-F20    TO       WK-TENPO-N.
     MOVE     DKC-F19    TO       WK-HACKBN.
*    取引先退避
     MOVE     DKC-F03    TO       WK-TORIHIKI.
***  MOVE     1          TO       TATE.
     MOVE     0          TO       TATE.
*
 INIT-EXT.
     EXIT.
*============================================================*
*　　センター納品データ読込み　　　　　　  構造■            *
*============================================================*
 DDCENTF-RD-SEC       SECTION.
     MOVE     "DDCENTF-RD-SEC"   TO   S-NAME.
*    センター納品データ読込み
     READ     DDCENTF  NEXT  AT  END
              IF READ-CNT > ZERO
                 MOVE      WK-GK-SURYO     TO     PRT-SOUGOK
                 PERFORM   SYUKEI-WT-SEC
              END-IF
              MOVE     "END"      TO       FLG-END
              GO                  TO       DDCENTF-RD-EXIT
     END-READ.
 READ010.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DKC-F01
     AND      PARA-JTIME  =  DKC-F02
     AND      PARA-TORICD =  DKC-F03
              CONTINUE
     ELSE
              IF READ-CNT > ZERO
                 MOVE      WK-GK-SURYO     TO     PRT-SOUGOK
                 PERFORM   SYUKEI-WT-SEC
              END-IF
              MOVE     "END"     TO   FLG-END
              GO                 TO   DDCENTF-RD-EXIT
     END-IF.
     IF       PARA-SOKO   =  SPACE
              GO                 TO   DDCENTF-RD-010
     END-IF.
*    抽出条件のチェック
     IF       PARA-SOKO      =    DKC-F04
              GO                  TO   DDCENTF-RD-010
     ELSE
              GO                  TO   DDCENTF-RD-SEC
     END-IF.
 DDCENTF-RD-010.
     IF       PARA-NOUST     <=   DKC-F05
     AND      PARA-NOUED     >=   DKC-F05
              CONTINUE
     ELSE
              GO                  TO   DDCENTF-RD-SEC
     END-IF.
 DDCENTF-RD-020.   *>センターＣＤと最終納品先が異なる場合
     IF       DKC-F06  =  DKC-F18
              CONTINUE
     ELSE
              GO                  TO   DDCENTF-RD-SEC
     END-IF.
 DDCENTF-RD-030.
     IF       PARA-TENST     <=   DKC-F06
     AND      PARA-TENED     >=   DKC-F06
              CONTINUE
     ELSE
              GO                  TO   DDCENTF-RD-SEC
     END-IF.
*    対象データ件数カウント
     ADD      1                   TO   READ-CNT.
*
 DDCENTF-RD-EXIT.
     EXIT.
*============================================================*
*　　メイン処理　　　　　　　　　　　　　  構造■2.0         *
*============================================================*
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
*    ブレイクチェック（納品日，店舗）
     IF       WK-NOUDATE  NOT =  DKC-F05
     OR       WK-TENPO    NOT =  DKC-F06
     OR       WK-HACKBN   NOT =  DKC-F19
              MOVE  WK-GK-SURYO  TO  PRT-SOUGOK
              PERFORM            SYUKEI-WT-SEC
              MOVE  DKC-F05      TO  WK-NOUDATE
              MOVE  DKC-F06      TO  WK-TENPO
              MOVE  DKC-F20      TO  WK-TENPO-N
              MOVE  DKC-F19      TO  WK-HACKBN
**            MOVE  1            TO  TATE
              MOVE  0            TO  TATE
              MOVE  ZERO         TO  WK-GK-SURYO
     END-IF.
*    店舗、納品日
     IF       WK-NOUDATE     =   DKC-F05
         AND  WK-TENPO       =   DKC-F06
              ADD   1            TO  TATE
*    同頁内で部門が２０アイテム以上の時，次頁へ改頁
              IF    TATE  >  20
                    PERFORM          SYUKEI-WT-SEC
                    MOVE  DKC-F05    TO  WK-NOUDATE
                    MOVE  DKC-F06    TO  WK-TENPO
                    MOVE  DKC-F20    TO  WK-TENPO-N
                    MOVE  DKC-F19    TO  WK-HACKBN
**                  MOVE  1          TO  TATE
                    MOVE  0          TO  TATE
                    MOVE  ZERO       TO  WK-GK-SURYO
              END-IF
     END-IF.
*    項目セット処理へ
     PERFORM  DATA-SET-SEC.
*    センター納品データ読込み
     PERFORM  DDCENTF-RD-SEC.
*
 MAIN-EXT.
     EXIT.
*============================================================*
*　　終了処理　　　　　　　　　　　　　　  構造■3.0         *
*============================================================*
 END-SEC                 SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*    使用ファイルＣＬＯＳＥ
     CLOSE               DDCENTF  HTENMS  PRTF.
*    終了メッセージ
     DISPLAY "************************" UPON  CONS.
     DISPLAY "*    荷個数明細書      *" UPON  CONS.
     DISPLAY "*  ｼｭﾂﾘｮｸ ｹﾝｽｳ = " PAGE-CNT "  *"  UPON  CONS.
     DISPLAY "************************" UPON  CONS.
*
 END-EXT.
     EXIT.
*============================================================*
*    荷個数明細書出力処理                  構造■2.1         *
*============================================================*
 SYUKEI-WT-SEC         SECTION.
     MOVE     "SYUKEI-WT-SEC"     TO   S-NAME.
*    ページ　カウントアップ
     ADD      1        TO        PAGE-CNT.
*****MOVE   PAGE-CNT   TO        PRT-HDPAGE.
*    システム日付セット
*****MOVE   WRK-DATE    TO       PRT-HDDATE.
*ヘッダ
     MOVE   WK-TORIHIKI           TO   TEN-F52.
     MOVE   WK-TENPO              TO   TEN-F011.
     READ   HTENMS
         INVALID   KEY
            MOVE ALL NC"＊" TO  PRT-CANTNM
            MOVE ALL NC"＊" TO  PRT-CANTN2
         NOT  INVALID  KEY
            MOVE TEN-F02(1:12) TO  PRT-CANTNM
            MOVE TEN-F02(13:3) TO  PRT-CANTN2
     END-READ.
     MOVE   WK-TENPO-N(1:12)   TO  PRT-CANTNM.
     MOVE   WK-TENPO-N(13:2)   TO  PRT-CANTN2.
*****MOVE    NC"定番"                 TO   PRT-KBN.
*
*発注種別変換マスタより取得
     MOVE     WK-TORIHIKI              TO   HSB-F01.
     MOVE     WK-HACKBN                TO   HSB-F02.
     PERFORM  DCMHSBL1-READ-SEC.
     IF  DCMHSBL1-INV-FLG  =  SPACE
              MOVE   HSB-F08           TO   PRT-KBN
     ELSE
              MOVE   ALL NC"＊"        TO   PRT-KBN
     END-IF.
*
*    納品日
     MOVE    DKC-F05(1:4)             TO   PRT-NOUYY.
     MOVE    DKC-F05(5:2)             TO   PRT-NOUMM.
     MOVE    DKC-F05(7:2)             TO   PRT-NOUDD.
     MOVE    WK-NOUDATE(1:4)          TO   PRT-NOUYY.
     MOVE    WK-NOUDATE(5:2)          TO   PRT-NOUMM.
     MOVE    WK-NOUDATE(7:2)          TO   PRT-NOUDD.
*    仕入先ＣＤ
     MOVE    PARA-TORICD              TO   PRT-TOKUCD.
     EVALUATE PARA-TORICD
         WHEN 1731
              MOVE    173             TO   PRT-TOKUCD
         WHEN 7601
              MOVE    760             TO   PRT-TOKUCD
         WHEN 1732
              MOVE    173             TO   PRT-TOKUCD
         WHEN 7602
              MOVE    760             TO   PRT-TOKUCD
         WHEN 139381
              MOVE    13938           TO   PRT-TOKUCD
         WHEN 171371
              MOVE    17137           TO   PRT-TOKUCD
         WHEN 100404
              MOVE    100403          TO   PRT-TOKUCD
         WHEN 100442
              MOVE    100441          TO   PRT-TOKUCD
         WHEN 100428
              MOVE    100427          TO   PRT-TOKUCD
         WHEN 882
              MOVE    880             TO   PRT-TOKUCD
         WHEN 883
              MOVE    880             TO   PRT-TOKUCD
         WHEN 14272
              MOVE    1427            TO   PRT-TOKUCD
         WHEN 14273
              MOVE    1427            TO   PRT-TOKUCD
         WHEN OTHER
              MOVE    PARA-TORICD     TO   PRT-TOKUCD
     END-EVALUATE.
*    仕入先名
     MOVE    NC"（株）サカタのタネ"   TO   PRT-TOKUNM.
*
*    印字制御項目セット
     MOVE     SPACE               TO   PRT-PROC.
     MOVE     SPACE               TO   PRT-CTL.
     MOVE     SPACE               TO   PRT-FORM.
     MOVE    "FSY05101"           TO   PRT-FORM.
     MOVE    "SCREEN"             TO   PRT-GRP.
*    荷個数明細書出力
 SYUKEI010.
     WRITE    PRT-FSY05101.
*    プリントエリア初期化
     MOVE     SPACE               TO   PRT-FSY05101.
*
 SYUKEI-WT-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造■2.2         *
*============================================================*
 DATA-SET-SEC            SECTION.
     MOVE     "DATA-SET-SEC"      TO   S-NAME.
*
*    明細
*    部門
     MOVE     DKC-F07        TO   PRT-BUMON(TATE).
     MOVE     DKC-F23        TO   PRT-BUMONM(TATE).
*
*    小物
     MOVE     DKC-F08                 TO   PRT-KOMONO(TATE).
*    異形
     MOVE     DKC-F09                 TO   PRT-IKEI(TATE).
*    ケース
     MOVE     DKC-F10                 TO   PRT-KESU(TATE).
*    大かご
     MOVE     DKC-F11                 TO   PRT-HOKA(TATE).
*    荷個数計
     COMPUTE  PRT-NIGOK(TATE) =  DKC-F08 + DKC-F09 +
                                 DKC-F10 + DKC-F11.
*
     ADD      PRT-NIGOK(TATE)    TO   WK-GK-SURYO.
*
 DATA-SET-EXIT.
     EXIT.
*#2019/03/19 NAV ST
****************************************************************
*　　発注種別変換マスタ索引
****************************************************************
 DCMHSBL1-READ-SEC         SECTION.
*
     READ     DCMHSBL1
         INVALID
           MOVE  "INV"     TO        DCMHSBL1-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        DCMHSBL1-INV-FLG
     END-READ.
*
 DCMHSBL1-READ-EXIT.
     EXIT.
*#2019/03/19 NAV ED

```
